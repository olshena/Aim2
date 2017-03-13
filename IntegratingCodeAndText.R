## @knitr klibraries 
library(randomForest)
library(rpart)
library(rpart.plot)

## @knitr kaim2
                            
aim2 <- function(dat,nreps=1,n.grid=20,mult=2,seed=12345,outvar="Y")  
{
  #Functions that go into penalized fitting method  
  aim2.list <- list(eval=aim2.eval, split=aim2.split, init=aim2.init, 
                    summary=aim2.summary, text=aim2.text)
  set.seed(seed)
  n <- nrow(dat)
  
  #Identify outcome variable   --- this is redundant with functions statement and should be changed
  which.outcome <- which(colnames(dat)==outvar)
  colnames(dat)[which.outcome] <- "outvar.aim2"
  
  #Split training set into learning and evaluation sets - now based on 50/50 split
  # later look at different alternatives
  nlearn <- round(n/2)
  neval <- n-nlearn
  samp <- sample(1:n,n,replace=FALSE)
  wlearn <- sort(samp[1:nlearn])
  weval <- sort(samp[(nlearn+1):n])
  learning.dat <- dat[wlearn,]
  evaluation.dat <- dat[weval,]

  #The lambdas chosen using a grid. Here, get values for variables based on root node
  fit.rf.learning <- randomForest(outvar.aim2 ~ .,data = learning.dat) # Fit RF with learning set
  predict.rf.evaluation <- predict(fit.rf.learning,newdata=evaluation.dat,
                                   predict.all=TRUE) #  $\widehat{Z}_{1i}$
  mean.evaluation <- mean(evaluation.dat$outvar.aim2) # $\mu_{Z_1}$
  var.evaluation <- var(evaluation.dat$outvar.aim2) # $\sigma^2_{Z_1}$
  zbarhat <- mean(predict.rf.evaluation$aggregate) # $ \bar{\hat{Z_1}}$ 
  
    # NOTE: this zbarhat means we are doing the 
    # alternative shrinkage target in equation 9 not the original in 6
  
  var.z1s <-  apply(predict.rf.evaluation$individual,1,var) # $\sigma^2_{\hat{Z_1i}}$
  alphas <- 1/var.z1s # $\alpha_i$
  alphabar <- mean(alphas) # \bar{\alpha}$
  lambda <- var.evaluation/(neval*alphabar*(mean.evaluation-zbarhat)^2) #with-in node 
                                                                        #choice of \lambda
  lambdas <- seq(0,mult*lambda,length.out=n.grid)  # list of possible lambdas
  n.lambdas <- length(lambdas) #length of list
  error.lambdas <- rep(0,length(lambdas)) 
  fits <- vector("list",n.lambdas)
  predictions <- vector("list",n.lambdas)
  
  # To get the err(\lambda) - uncorrected - currently equation 11
  for(j in 1:n.lambdas)
    {
      current.fit <- rpart(outvar.aim2 ~ .,data = evaluation.dat,
                           parms=list(lambda=lambdas[j],
                           yhat=predict.rf.evaluation$aggregate,
                           alpha=alphas),method=aim2.list)   
      predicted.fit <- predict(object=current.fit,newdata=evaluation.dat)
      error.lambdas[j] <- sum((evaluation.dat$outvar.aim2-predicted.fit)^2)
      fits[[j]] <- current.fit
      predictions[[j]] <- predicted.fit
  }
  
  # To get the optimism for correcting the err(\lambda)
  optimism <- corrected.lambda(dat=evaluation.dat,lambdas=lambdas,
                               list.object=aim2.list,model=fit.rf.learning,
                               predicted.values=predict.rf.evaluation$aggregate,
                               alphas=alphas,n.boot=10)
  
  # err(\lambda) corrected
  Error.lambdas <- error.lambdas+optimism
  
  #return list of interesting variables.
  list(lambdas=lambdas,Error.lambdas=Error.lambdas,error.lambdas=error.lambdas,
       optimism=optimism,fits=fits,predictions=predictions,
       predicted.fit=predicted.fit,evaluation.dat=evaluation.dat)
}

## @knitr kcorrected.lambda 
corrected.lambda <- function(dat,lambdas,list.object,model,predicted.values,alphas,n.boot=10)
  {
    n1 <- nrow(dat)
    p <- ncol(dat)
    n.lambdas <- length(lambdas)
    cilambda <- matrix(0,n1,n.lambdas)
    boot.dat <- boot.residual <- matrix(NA,n1,n.boot)
    sigmahat <- sqrt(sum((dat$outvar.aim2-predicted.values)^2)/n1) #SD for \hat{\sigma^2_{Z_1-\bigM}}

    for(b in 1:n.boot) boot.dat[,b] <- rnorm(n1,mean=predicted.values,sd=sigmahat)#bootstrap samples
    boot.mean <- matrix(apply(boot.dat,1,mean))  # \bar{Z^*_{1i}}
    for(i in 1:nrow(boot.dat)) boot.residual[i,] <- boot.dat[i,]-boot.mean[i]
    for(b in 1:n.boot)
      {
        new.dat <- dat
        new.dat$outvar.aim2 <- boot.dat[,b] 
        for(j in 1:n.lambdas)
          {
            final.fit <- rpart(outvar.aim2 ~ .,data = new.dat,
                               parms=list(lambda=lambdas[j],
                               yhat=predicted.values,alpha=alphas),
                               method=list.object)
            bigMhat <- predict(object=final.fit,newdata=new.dat)
            cilambda[,j] <- cilambda[,j]+bigMhat*boot.residual[,b]
          }
      }
    cilambda <- cilambda/(n.boot-1)
    return(2*apply(cilambda,2,sum))
  }

## @knitr krpartfxs


#y contains response Z_i, Zhat_i from RF, and alpha where alpha=1/var(zhat)

aim2.init <- function(y, offset, parms, wt)
{
  if (!is.null(offset)) y[,1] <- y[,1]-offset
  list(y=cbind(y,parms$yhat,parms$alpha),parms=parms, numy=3, numresp=1, summary=aim2.summary)
}

aim2.eval <- function(y, wt, parms)
{
  n <- length(y)/3
  lambda <- parms$lambda
  yhat <- y[,2]
  alphas <- y[,3]
  alphabar <- sum(alphas)/n
  y1 <- y[,1]
  r <- 1/(1+lambda*alphabar)
  zbar <- mean(y1)
  zbarhat <- sum(yhat*alphas)/sum(alphas)
  chat <- r*zbar+(1-r)*zbarhat
  rss <- sum((y1-chat)^2+lambda*alphas*(chat-yhat)^2)
  list(label=chat, deviance=rss)
}

aim2.split <- function(y, wt, x, parms, continuous)
  {
    n <- length(y[,1])
    y1 <- y[,1]
    yhat <- y[,2]
    alpha <- y[,3]
    lambda <- parms$lambda
    if (continuous)
      {
        if(is.null(lambda)) compute.lambda #Placeholder until I figure out how to compute lambda
        goodness <- direction <- double(n-1) #Allocate 0 vector
        y.cumsum <- cumsum(y1)
        y.left <- y.cumsum[-n]
        y.right <- y.cumsum[n]-y.left
        yhat.cumsum <- cumsum(yhat*alpha)
        yhat.left <- yhat.cumsum[-n]
        yhat.right <- yhat.cumsum[n]-yhat.left
        alpha.cumsum <- cumsum(alpha)
        alpha.left <- alpha.cumsum[-n]
        alpha.right <- alpha.cumsum[n]-alpha.left
        for(i in 1:(n-1))
          {
            zbar.left <- y.left[i]/i
            zbar.right <- y.right[i]/(n-i)
            zbarhat.left <- yhat.left[i]/alpha.left[i]
            zbarhat.right <- yhat.right[i]/alpha.right[i]
            alphabar.left <- alpha.left[i]/i
            alphabar.right <- alpha.right[i]/(n-i)
            r.left <- 1/(1+lambda*alphabar.left)
            r.right <- 1/(1+lambda*alphabar.right)
            chat.left <- r.left*zbar.left+(1-r.left)*zbarhat.left
            chat.right <- r.right*zbar.right+(1-r.right)*zbarhat.right

#            goodness[i] <- sum((y1-mean(y1))^2) 
#                 - (sum((y1[1:i]-chat.left)^2 +
#                    lambda*alpha[1:i]*(yhat[1:i]-chat.left)^2) +
#                   sum((y1[(i+1):n]-chat.right)^2 +
#                  lambda*alpha[(i+1):n]*(yhat[(i+1):n]-chat.right)^2)) 
#           Do we need adjustment for missing values like in  vignette example?
            
            direction[i] <- sign(zbar.left-zbar.right)
            goodness.left <- sum((y1[1:i]-chat.left)^2 + lambda*alpha[1:i]*(yhat[1:i]-chat.left)^2)
            goodness.right <- sum((y1[(i+1):n]-chat.right)^2 + 
                                    lambda*alpha[(i+1):n]*(yhat[(i+1):n]-chat.right)^2)
            tss <- sum((y1-mean(y1))^2)
            goodness[i] <- tss-goodness.left-goodness.right

          }
      } # this means we can only have x continuous - no categorical
#    goodness <- 1/goodness
    return(list(goodness=goodness, direction=direction))
  }

aim2.summary <- function(yval, dev, wt, ylevel, digits )
{
  paste(" mean=", format(signif(yval, digits)), ", MSE=" , format(signif(dev/wt, digits)), sep= '')
}

aim2.text <- function(yval, dev, wt, ylevel, digits, n, use.n )
{
  if(use.n) paste(formatg(yval,digits)," nn=", n,sep="")
  else paste(formatg(yval,digits))
}



