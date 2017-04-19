## @knitr klibraries 
library(randomForest)
library(rpart)
library(rpart.plot)
library(logspline)

## @knitr kaim2
                            
aim2 <- function(dat,nreps=1,n.grid=20,mult=2,outvar="Y",prop.learning=0.5)  
{
  #Functions that go into penalized fitting method  
  aim2.list <- list(eval=aim2.eval, split=aim2.split, init=aim2.init, 
                    summary=aim2.summary, text=aim2.text)
  n <- nrow(dat)
  
  #Identify outcome variable   --- this is redundant with functions statement and should be changed
  which.outcome <- which(colnames(dat)==outvar)
  colnames(dat)[which.outcome] <- "outvar.aim2"
  
  #Split training set into learning and evaluation sets - now based on 50/50 split
  # later look at different alternatives
  nlearn <- round(prop.learning*n)
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
  #print("almost there")
  # To get the err(\lambda) - uncorrected - currently equation 11
  for(j in 1:n.lambdas)
    {
      #print(lambdas[j])
      current.fit <- rpart(outvar.aim2 ~ .,data = evaluation.dat,
                           parms=list(lambda=lambdas[j],
                           yhat=predict.rf.evaluation$aggregate,
                           alpha=alphas),method=aim2.list)  
      xgroup <- rep(1:10, length = nrow(evaluation.dat))
      xfit <- xpred.rpart(current.fit,xgroup)
      xerror <- colMeans((xfit - evaluation.dat$outvar.aim2)^2)
      min.CP<-current.fit$cptable[which(xerror==min(xerror)),1][1]
      current.fit.pruned<-prune(current.fit,cp=min.CP)
      predicted.fit <- predict(object=current.fit.pruned,newdata=evaluation.dat)
      error.lambdas[j] <- sum((evaluation.dat$outvar.aim2-predicted.fit)^2)
      fits[[j]] <- current.fit.pruned
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
            xgroup <- rep(1:10, length = nrow(new.dat))
            xfit <- xpred.rpart(final.fit,xgroup)
            xerror <- colMeans((xfit - new.dat$outvar.aim2)^2)
            min.CP<-final.fit$cptable[which(xerror==min(xerror)),1][1]
            final.fit.pruned<-prune(final.fit,cp=min.CP)
            
            bigMhat <- predict(object=final.fit.pruned,newdata=new.dat)
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

## @knitr kcomposite

composite.rpart=function(dat,n.grid=20,mult=2,outvar="Y",prop.learning=0.5)
{
  n <- nrow(dat)
  which.outcome <- which(colnames(dat)==outvar)
  colnames(dat)[which.outcome] <- "outvar.aim2"	

  #Split into learning and evaluation sets 
  nlearn <- round(prop.learning*n)
  neval <- n-nlearn
  samp <- sample(1:n,n,replace=FALSE)
  wlearn <- sort(samp[1:nlearn])
  weval <- sort(samp[(nlearn+1):n])
  learning.dat <- dat[wlearn,]
  evaluation.dat <- dat[weval,]

  fit.rf.learning <- randomForest(outvar.aim2 ~ .,data = learning.dat) # Fit RF with learning set
  predict.rf.evaluation <- predict(fit.rf.learning,newdata=evaluation.dat,predict.all=TRUE) #  $\widehat{Z}_{1i}$
  mean.evaluation <- mean(evaluation.dat$outvar.aim2) # $\mu_{Z_1}$
  var.evaluation <- var(evaluation.dat$outvar.aim2) # $\sigma^2_{Z_1}$
  zbarhat <- mean(predict.rf.evaluation$aggregate) # $ \bar{\hat{Z_1}}$ 
  var.z1s <-  apply(predict.rf.evaluation$individual,1,var) # $\sigma^2_{\hat{Z_1i}}$
  alphas <- 1/var.z1s # $\alpha_i$
  alphabar <- mean(alphas) # \bar{\alpha}$
  lambda <- var.evaluation/(neval*alphabar*(mean.evaluation-zbarhat)^2) #with-in node 
                                                                        #choice of \lambda
  #print(paste("lambda =",lambda)) 									
  lambdas <- seq(0,mult*lambda,length.out=n.grid)  # list of possible lambdas
  n.lambdas <- length(lambdas) #length of list
  error.lambdas <- rep(0,length(lambdas)) 
  fits <- vector("list",n.lambdas)
  predictions <- vector("list",n.lambdas)
  use.dat<-evaluation.dat
  # To get the err(\lambda) - uncorrected - currently equation 11
  for(j in 1:n.lambdas)
    {
      new.lambda <- lambdas[j]
      #print(new.lambda)
      new.denom <- (1+alphas*new.lambda)
      #print(new.denom)
      ri <- 1/new.denom
      ci <- ri*use.dat$outvar.aim2 + (1-ri)*predict.rf.evaluation$aggregate
      new.use.dat <- use.dat
      new.use.dat$outvar.aim2 <- ci
      current.fit <- rpart(outvar.aim2 ~ .,data = new.use.dat)
      min.CP<-current.fit$cptable[which(current.fit$cptable[,4]==min(current.fit$cptable[,4])),1][1]
      current.fit.pruned<-prune(current.fit,cp=min.CP)
      predicted.fit <- predict(object=current.fit.pruned, data=new.use.dat)
      error.lambdas[j] <- sum((new.use.dat$outvar.aim2-predicted.fit)^2)
      fits[[j]] <- current.fit
      predictions[[j]] <- predicted.fit
    }

  list(lambda=lambda,lambdas=lambdas,error.lambdas=error.lambdas,fits=fits,predictions=predictions)
}


#housing.data <- as.data.frame(matrix(scan("housing.data"),nrow=506,byrow=TRUE))
#colnames(housing.data) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad",
#                            "tax","ptratiob","b","lstat","mdev")
#set.seed(12345)			    
#temp <- composite.rpart(dat=housing.data,n.grid=20,outvar="mdev")
#plot(temp$lambdas,temp$Error.lambdas,xlab="lambda",ylab="Optimism corrected error",
#     main="Optimism corrected error vs. lambda for housing data")

## @knitr kcomposite.thirds

composite.rpart.thirds <- function(dat,n.grid=20,mult=2,outvar="Y")
{
  n <- nrow(dat)
  which.outcome <- which(colnames(dat)==outvar)
  colnames(dat)[which.outcome] <- "outvar.aim2"	

  #Split into learning and evaluation sets 
  nlearn <- round(n/3)
  ndisc <- round(n/3)
  neval <- n-nlearn-ndisc
  samp <- sample(1:n,n,replace=FALSE)
  wlearn <- sort(samp[1:nlearn])
  wdisc <- sort(samp[(nlearn+1):(nlearn+ndisc)])
  weval <- sort(samp[(nlearn+ndisc+1):n])
  
  learning.dat <- dat[wlearn,]
  discovery.dat <- dat[wdisc,]
  evaluation.dat <- dat[weval,]

  fit.rf.learning <- randomForest(outvar.aim2 ~ .,data = learning.dat) # Fit RF with learning set
  predict.rf.discovery <- predict(fit.rf.learning,newdata=discovery.dat,predict.all=TRUE) #  $\widehat{Z}_{1i}$
  mean.discovery <- mean(discovery.dat$outvar.aim2) # $\mu_{Z_1}$
  var.discovery <- var(discovery.dat$outvar.aim2) # $\sigma^2_{Z_1}$
  zbarhat <- mean(predict.rf.discovery$aggregate) # $ \bar{\hat{Z_1}}$ 
  var.z1s <-  apply(predict.rf.discovery$individual,1,var) # $\sigma^2_{\hat{Z_1i}}$
  alphas <- 1/var.z1s # $\alpha_i$
  alphas <- alphas/sum(alphas)
  alphabar <- mean(alphas) # \bar{\alpha}$
  lambda <- var.discovery/(ndisc*alphabar*(mean.discovery-zbarhat)^2) #with-in node 
                                                                        #choice of \lambda
  print(paste("zbarhat =",zbarhat))
  print(paste("mean.discovery =",mean.discovery))
  print(paste("alphabar =",alphabar))
  print(paste("neval =",neval))		  
  print(paste("var.discovery =",var.discovery))
  print(paste("lambda =",lambda)) 									  
  lambdas <- seq(0,mult*lambda,length.out=n.grid)  # list of possible lambdas
  n.lambdas <- length(lambdas) #length of list
  error.lambdas <- rep(0,n.lambdas) 
  fits <- vector("list",n.lambdas)
  pruneds <- vector("list",n.lambdas)
  predictions <- vector("list",n.lambdas)
  use.dat<-discovery.dat
  # To get the err(\lambda) - uncorrected - currently equation 11
  for(j in 1:n.lambdas)
    {
      new.lambda <- lambdas[j]
      new.denom <- (1+alphas*new.lambda)
      ri <- 1/new.denom
      ci <- ri*use.dat$outvar.aim2 + (1-ri)*predict.rf.discovery$aggregate
      new.use.dat <- use.dat
      new.use.dat$outvar.aim2 <- ci
      current.fit <- rpart(outvar.aim2 ~ .,data = new.use.dat)
      min.CP<-current.fit$cptable[which(current.fit$cptable[,4]==min(current.fit$cptable[,4])),1][1]
      current.fit.pruned<-prune(current.fit,cp=min.CP)
      predicted.fit <- predict(object=current.fit.pruned, data=evaluation.dat)
      error.lambdas[j] <- sum((evaluation.dat$outvar.aim2-predicted.fit)^2)
      fits[[j]] <- current.fit
      pruneds[[j]] <- current.fit.pruned
      predictions[[j]] <- predicted.fit
    }
  best.lambda <- lambdas[order(error.lambdas)[1]]
  new.denom <- (1+alphas*best.lambda)
  ri <- 1/new.denom
  ci <- ri*dat$outvar.aim2 + (1-ri)*predict(fit.rf.learning,newdata=dat)
  dat$outvar.aim2 <- ci
  current.fit <- rpart(outvar.aim2 ~ .,data = dat)
  min.CP<-current.fit$cptable[which(current.fit$cptable[,4]==min(current.fit$cptable[,4])),1]
  current.fit.pruned<-prune(current.fit,cp=min.CP)
  
  list(best.lambda=best.lambda,lambda=lambda,lambdas=lambdas,error.lambdas=error.lambdas,fits=fits,pruneds=pruneds,predictions=predictions,current.fit=current.fit,current.fit.pruned=current.fit.pruned,alphas=alphas,alphas.lambda=alphas*best.lambda,ri=ri)
}

## @knitr RobsComposite


# R version of Matlab function 'repmat'. E.g., if given a
# vector 'X' of length 'k', then repmat(X,1,n) will give the
# k x n matrix where each matrix column is a copy of X.


repmat = function(X, m, n) {
  if (is.vector(X)) 
    X = matrix(X, length(X), 1)
  mx = dim(X)[1]
  nx = dim(X)[2]
  return(matrix(t(matrix(X, mx, nx * n)), mx * m, nx * n, byrow = T))
}

# Use leave-one-out CV (LOO)
CVcorrected.lambda <- function(dat, lambdas, model, predicted.values, 
                               alphas) {
  n1 <- nrow(dat)
  p <- ncol(dat)
  n.lambdas <- length(lambdas)
  cilambda <- matrix(0, n1, n.lambdas)
  
  for (b in 1:n1) {
    # create new LOO dataset that deletes bth observation
    new.dat <- dat[-b, ]
    
    # for bth LOO dataset, loop over lambda
    for (j in 1:n.lambdas) {
      # derive modified response
      new.lambda <- lambdas[j]
      ri <- 1/(1 + alphas[-b] * new.lambda)
      ci <- ri * new.dat$outvar.aim2 + (1 - ri) * predicted.values[-b]
      
      # create modified LOO dataset
      use.dat = new.dat
      use.dat$outvar.aim2 = ci
      
      # fit model to modified LOO dataset
      final.fit <- rpart(outvar.aim2 ~ ., data = use.dat)
      
      ####################################################### prune this fit
      xgroup <- rep(1:10, length = nrow(use.dat))
      xfit <- xpred.rpart(final.fit, xgroup)
      
      # here -- calculates error using original bootstrap response
      # (new.dat) and not use.dat (weighted response)
      xerror <- colMeans((xfit - new.dat$outvar.aim2)^2)
      
      ff = which(xerror == min(xerror))[1]
      min.CP <- final.fit$cptable[ff, 1]
      final.fit.pruned <- prune(final.fit, cp = min.CP)
      ####################################################### 
      
      
      # calculate the predicted value for each subject in dat from
      # new LOO model built using dat[-b,]
      bigMhat <- predict(object = final.fit.pruned, newdata = dat)
      
      # get Zb - tree(-b,Wb) for the bth LOO at lambda = lambda[j]
      # and square the result
      diffpred = (dat$outvar.aim2 - bigMhat)[b]
      cilambda[b, j] = diffpred^2
    }
  }
  return(apply(cilambda, 2, sum))
}


# parametric bootstrap - this has been modified from Adam's
# original version

corrected.lambda.LSD <- function(dat, lambdas, list.object, model, 
                             predicted.values, alphas, n.boot = 10) {
  n1 <- nrow(dat)
  p <- ncol(dat)
  n.lambdas <- length(lambdas)
  cilambda <- matrix(0, n1, n.lambdas)
  
  # bootstrap the data - generate new 'Z's about \hat{Z}
  
  # note: the code below is equivalent but faster than original
  # bootstrap code that used normal errors sigmahat <-
  # sqrt(sum((dat$outvar.aim2-predicted.values)^2)/n1) boot.dat
  # =
  # matrix(rnorm(n.boot*n1,mean=predicted.values,sd=sigmahat),n1,n.boot)
  # boot.mean <- apply(boot.dat,1,mean) boot.residual =
  # boot.dat-boot.mean
  
  # here, use a nonparametric density estimator for the errors
  # 'Zi-\hat{Zi}' and then generate bootstrapped Z by adding
  # random error to \hat{Zi}
  yy = dat$outvar.aim2 - predicted.values
  fit = logspline(yy)
  booterr.dat = matrix(rlogspline(n.boot * n1, fit), n1, n.boot)
  boot.dat = repmat(predicted.values, 1, n.boot) + booterr.dat
  boot.mean <- repmat(as.vector(apply(boot.dat, 1, mean)), 
                      1, n.boot)
  boot.residual = boot.dat - boot.mean
  for (b in 1:n.boot) {
    new.dat <- dat
    new.dat$outvar.aim2 <- boot.dat[, b]
    
    for (j in 1:n.lambdas) {
      new.lambda <- lambdas[j]
      ri <- 1/(1 + alphas * new.lambda)
      ci <- ri * new.dat$outvar.aim2 + (1 - ri) * predicted.values
      use.dat = new.dat
      use.dat$outvar.aim2 = ci
      
      # fit model to bootstrapped weighted response
      final.fit <- rpart(outvar.aim2 ~ ., data = use.dat)
      
      ####################################################### prune the bootstrap fit
      xgroup <- rep(1:10, length = nrow(use.dat))
      xfit <- xpred.rpart(final.fit, xgroup)
      
      # here -- calculates error using original bootstrap response
      # (new.dat) and not use.dat (weighted response)
      xerror <- colMeans((xfit - new.dat$outvar.aim2)^2)
      
      ff = which(xerror == min(xerror))[1]
      min.CP <- final.fit$cptable[ff, 1]
      final.fit.pruned <- prune(final.fit, cp = min.CP)
      ####################################################### 
      
      # calculate the predicted value for each subject in use.dat;
      # note that it does not matter whether use.dat or new.dat is
      # used here since the response is not used in deriving model
      # prediction
      bigMhat <- predict(object = final.fit.pruned, newdata = use.dat)
      
      # for lambda=lambda[j], add bootstrap contributions to
      # covariance penalty from bth sample. The boot.residual is
      # the bootstrapped response, not weighted response. Also,
      # note that this is numerically equivalent to calculation
      # that replaces 'bigMhat' with 'bigMhat-predicted.values'
      
      cilambda[, j] <- cilambda[, j] + bigMhat * boot.residual[, 
                                                               b]
      
    }
  }
  cilambda <- cilambda/(n.boot - 1)
  return(2 * apply(cilambda, 2, sum))
}

composite.rpart.Grid = function(dat, n.grid = 20, mult = 1, uplim = 10, 
                           outvar = "Y", prop.learning = 0.5) {
  
  n <- nrow(dat)
  which.outcome <- which(colnames(dat) == outvar)
  colnames(dat)[which.outcome] <- "outvar.aim2"
  
  # Split into learning and evaluation sets
  nlearn <- round(prop.learning * n)
  neval <- n - nlearn
  samp <- sample(1:n, n, replace = FALSE)
  wlearn <- sort(samp[1:nlearn])
  weval <- sort(samp[(nlearn + 1):n])
  learning.dat <- dat[wlearn, ]
  evaluation.dat <- dat[weval, ]
  
  # use learning set to derive RF predictions
  fit.rf.learning <- randomForest(outvar.aim2 ~ ., data = learning.dat, 
                                  ntree = 1000)
  predict.rf.evaluation <- predict(fit.rf.learning, newdata = evaluation.dat, 
                                   predict.all = TRUE)

  # mean, variance of Z's in evaluation set
  mean.evaluation <- mean(evaluation.dat$outvar.aim2)  # $\mu_{Z_1}$
  var.evaluation <- var(evaluation.dat$outvar.aim2)  # $\sigma^2_{Z_1}$
  
  # mean, variance of RF predictions for evaluation set
  zbarhat <- mean(predict.rf.evaluation$aggregate)  # $ \bar{\hat{Z_1}}$ 
  var.z1s <- apply(predict.rf.evaluation$individual, 1, var)  # $\sigma^2_{\hat{Z_1i}}$
  
  # Hard to know how to chooose. Need to think about objective.
  # But sensible choice in that high variance predictions would
  # get lower weight
  alphas <- 1/var.z1s
  # alphas = rep(1,length(var.z1s))
  
  # 'optimal' 'root node' lambda for specified alphas - not
  # really relevant here since calculated using a different
  # objective. Could revisit this calculation. alphabar <-
  # mean(alphas) # \bar{\alpha}$ lambda <-
  # var.evaluation/(neval*alphabar*(mean.evaluation-zbarhat)^2)
  # print(paste('lambda =',lambda))
  
  # grid of lambda values
  lambdas <- seq(0, uplim, length.out = n.grid)  # list of possible lambdas
  n.lambdas <- length(lambdas)  #length of list
  
  error.lambdas <- rep(0, length(lambdas))
  errorU.lambdas <- rep(0, length(lambdas))
  fits <- vector("list", n.lambdas)
  predictions <- vector("list", n.lambdas)
  
  use.dat <- evaluation.dat
  
  for (j in 1:n.lambdas) {
    
    new.lambda <- lambdas[j]
    new.denom <- (1 + alphas * new.lambda)
    ri <- 1/new.denom
    
    # weighted response calculation
    ci <- ri * use.dat$outvar.aim2 + (1 - ri) * predict.rf.evaluation$aggregate
    
    # build tree using dataset that employs weighted response
    new.use.dat <- use.dat
    new.use.dat$outvar.aim2 <- ci
    current.fit <- rpart(outvar.aim2 ~ ., data = new.use.dat)
    
    # prune that tree using manual CV
    min.CP <- current.fit$cptable[which(current.fit$cptable[, 
                                                            4] == min(current.fit$cptable[, 4])), 1][1]
    current.fit.pruned <- prune(current.fit, cp = min.CP)
    
    # derive predicted outcomes for each observation in that
    # dataset
    predicted.fit <- predict(object = current.fit.pruned, 
                             data = new.use.dat)
    
    # calculate 'apparent error' using 'ci' as response
    error.lambdas[j] <- sum((new.use.dat$outvar.aim2 - predicted.fit)^2)
    
    # calculate 'apparent error' using actual response
    errorU.lambdas[j] <- sum((use.dat$outvar.aim2 - predicted.fit)^2)
    
    
    fits[[j]] <- current.fit
    predictions[[j]] <- predicted.fit
  }
  
  CVError.lambdas <- CVcorrected.lambda(dat = evaluation.dat, 
                                        lambdas = lambdas, model = fit.rf.learning, predicted.values = predict.rf.evaluation$aggregate, 
                                        alphas = alphas)
  
  Error.lambdas <- errorU.lambdas
  optimism <- corrected.lambda.LSD(dat = evaluation.dat, lambdas = lambdas, 
                               list.object = aim2.list, model = fit.rf.learning, predicted.values = predict.rf.evaluation$aggregate, 
                               alphas = alphas, n.boot = 1000)
  Error.lambdas <- Error.lambdas + optimism
  
  list(lambdas = lambdas, error.lambdas = error.lambdas, errorU.lambdas = errorU.lambdas, 
       Error.lambdas = Error.lambdas, fits = fits, predictions = predictions, 
       optimism = optimism, CVError.lambdas = CVError.lambdas)
}
