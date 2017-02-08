#Implement interpretable trees code 10/17/2015

library(randomForest)
library(rpart)

#y contains response, yhat from tree, and alpha where alpha=1/var(zhat)

aim2.init <- function(y, offset, parms, wt)
{
  if (!is.null(offset)) y[,1] <- y[,1]-offset
  list(y=cbind(y,parms$yhat,parms$alpha),parms=parms, numy=3, numresp=1, summary=aim2.summary)
}

aim2.eval <- function(y, wt, parms)
{
#  print("In eval")
  n <- length(y)/3
#  print("neval"); print(n)
#  print("y")
#  print(y)
  lambda <- parms$lambda
  yhat <- y[,2]
#  print("yeval");  print(y[1:10,1])
#  print("yhateval");  print(parms$yhat[1:10])  
  alphas <- y[,3]
#  print("alphainit");  print(parms$alpha[1:10])    
  alphabar <- sum(alphas)/n
  y1 <- y[,1]
  r <- 1/(1+lambda*alphabar)
#  print("lambda");print(lambda)
#  print("r");print(r)
  zbar <- mean(y1)
  zbarhat <- sum(yhat*alphas)/sum(alphas)
  chat <- r*zbar+(1-r)*zbarhat
#  print("chat"); print(chat)
  rss <- sum((y1-chat)^2+lambda*alphas*(chat-yhat)^2)
#  print("rss"); print(rss)
  list(label=chat, deviance=rss)
}

aim2.split <- function(y, wt, x, parms, continuous)
  {
#    print("In split")
#    print(y[1,])
    n <- length(y[,1])
#    print(n)
    y1 <- y[,1]
#    print("y1")
#    print(y1[1:10])
    yhat <- y[,2]
#    print("yhat")
#    print(yhat[1:10])
    alpha <- y[,3]
#    print("alpha")
#    print(alpha[1:10])
    lambda <- parms$lambda
#    print("lambda")
#    print(lambda)
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
#            print(paste("i=",i,sep=""))
            zbar.left <- y.left[i]/i
#            print("zbar.left")
#            print(zbar.left)
            zbar.right <- y.right[i]/(n-i)
#            print("zbar.right")
#            print(zbar.right)
            zbarhat.left <- yhat.left[i]/alpha.left[i]
            zbarhat.right <- yhat.right[i]/alpha.right[i]
            alphabar.left <- alpha.left[i]/i
            alphabar.right <- alpha.right[i]/(n-i)
            r.left <- 1/(1+lambda*alphabar.left)
            r.right <- 1/(1+lambda*alphabar.right)
            chat.left <- r.left*zbar.left+(1-r.left)*zbarhat.left
            chat.right <- r.right*zbar.right+(1-r.right)*zbarhat.right
#            print(paste("length of lambda",length(lambda)))
#            print(paste("length of alphabar.left",length(alphabar.left)))
#            print(paste("length of alphabar.right",length(alphabar.right)))
#            print(paste("length of r.left",length(r.left)))
#            print(paste("length of r.right",length(r.right)))
#            goodness[i] <- sum((y1-mean(y1))^2) - (sum((y1[1:i]-chat.left)^2 +
#                                                       lambda*alpha[1:i]*(yhat[1:i]-chat.left)^2) +
#                                                   sum((y1[(i+1):n]-chat.right)^2 +
#                                                       lambda*alpha[(i+1):n]*(yhat[(i+1):n]-chat.right)^2)) #Do we need adjustment for missing values like in  vignette example?
            direction[i] <- sign(zbar.left-zbar.right)
            goodness.left <- sum((y1[1:i]-chat.left)^2 + lambda*alpha[1:i]*(yhat[1:i]-chat.left)^2)
            goodness.right <- sum((y1[(i+1):n]-chat.right)^2 + lambda*alpha[(i+1):n]*(yhat[(i+1):n]-chat.right)^2)
            tss <- sum((y1-mean(y1))^2)
            goodness[i] <- tss-goodness.left-goodness.right
#            if(i==16)           print(paste("i=",i,",x=",x[i],",chat.left=",chat.left,",chat.right=",chat.right,",goodness.left=",goodness.left,",goodness.right=",goodness.right,",tss=",tss,",goodness=",goodness[i],sep=""))
          }
      }
#    goodness <- 1/goodness
#    print(paste("position=",order(goodness)[1],sep=""))
#    print(paste("min(goodness)=",min(goodness),sep=""))
#    print(paste("max(goodness)=",max(goodness),sep=""))
#    print("y1[1:16]")
#    print(y1[1:16])
#    print("direction"); print(direction)
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

find.lambda <- function(dat,lambdas,list.object)
  {
#Now do three-fold cross-validation to choose among lambdas  
    n <- nrow(dat)
    mod <- n%/%3
    div <- n%%3
    n1 <- n2 <- n3 <- mod
    if(div==1) n1 <- n1+1
    else if (div==2) n1 <- n1+1; n2 <- n2+1
    samp <- sample(1:n,n,replace=FALSE)
    which1 <- sort(samp[1:n1])
    which2 <- sort(samp[(n1+1):(n1+n2)])
    which3 <- sort(samp[(n1+n2+1):n])
    dat1 <- dat[which1,]
    dat2 <- dat[which2,]
    dat3 <- dat[which3,]
    ngrid <- length(lambdas)
    aim2.fits <- vector("list",ngrid)
    aim2.predictions <- vector("list",ngrid)
    for(i in 1:ngrid)
      {
        aim2.fits[[i]] <- vector("list",3)
        aim2.predictions[[i]] <- rep(NA,n)
      }
#3-fold
    for(i in 1:3)
      {
#       print(i) 
        if(i==1){tr.dat <- dat1; te.dat <- dat2; val.dat <- dat3; new.validation <- which3}
        else if(i==2){tr.dat <- dat2; te.dat <- dat3; val.dat <- dat1; new.validation <- which1}
        else if(i==3){tr.dat <- dat3; te.dat <- dat1; val.dat <- dat2; new.validation <- which2}
        fit.rf.tr <- randomForest(outvar.aim2 ~ .,data = tr.dat)
        predict.rf.te <- predict(fit.rf.tr,newdata=te.dat,predict.all=TRUE)
        var.z1s.3 <-  apply(predict.rf.te$individual,1,var)
        alphas.3 <- 1/var.z1s.3
        alphabar.3 <- mean(alphas.3)
#Now loop through lambdas
        for(j in 1:ngrid)
          {
            new.fit <- rpart(outvar.aim2 ~ .,data = te.dat,parms=list(lambda=lambdas[j],yhat=predict.rf.te$aggregate,alpha=alphas.3),method=list.object)
            aim2.fits[[j]][[i]] <- new.fit
            new.predictions <- predict(new.fit,newdata=val.dat,predict.all=TRUE)
#          print(length(new.predictions))
#          print(length(new.validation))
# if(j==1)         print(sort(new.validation))
            aim2.predictions[[j]][new.validation] <- new.predictions
          }
      }
    rss <- rep(NA,ngrid)
    for(i in 1:ngrid) rss[i] <- sum((dat$outvar.aim2-aim2.predictions[[i]])^2)
    final.lambda <- lambdas[order(rss)[1]]
    final.lambda
  }

corrected.lambda <- function(dat,lambdas,list.object,model,predicted.values,alphas,n.boot=10)
  {
    n1 <- nrow(dat)
    p <- ncol(dat)
    n.lambdas <- length(lambdas)
    cilambda <- matrix(0,n1,n.lambdas)
    boot.dat <- boot.residual <- matrix(NA,n1,n.boot)
    sigmahat <- sqrt(sum((dat[,p]-predicted.values)^2)/n1)
#    print(predicted.values)
#    print(sigmahat)
    for(i in 1:n.boot) boot.dat[,i] <- rnorm(n1,mean=predicted.values,sd=sigmahat)
    boot.mean <- matrix(apply(boot.dat,1,mean))
    for(i in 1:nrow(boot.dat))
        {
            boot.residual[i,] <- boot.dat[i,]-boot.mean[i]
#            print(boot.residual[i,])
        }
    for(i in 1:n.boot)
      {
        new.dat <- dat
        new.dat[,p] <- boot.dat[,i] 
        for(j in 1:n.lambdas)
          {
            final.fit <- rpart(outvar.aim2 ~ .,data = new.dat,parms=list(lambda=lambdas[j],yhat=predicted.values,alpha=alphas),method=list.object)
            muhat <- predict(object=final.fit,newdata=new.dat)
            cilambda[,j] <- cilambda[,j]+muhat*boot.residual[,i]
          }
      }
    cilambda <- cilambda/(n.boot-1)
    output <- 2*apply(cilambda,2,sum)
    output
  }

#dat is data frame to which model is fit
#nreps is not used right now
#ngrid is the number of lambdas in the grid search
#mult is the number multiplied times the intial lambda the is the maximum lambda in the grid search
#seed fixes the random number generator for reproducibility
#outvar is the name of the outcome variable in the fitting
                             
aim2 <- function(dat,nreps=1,n.grid=20,mult=2,seed=12345,outvar="mdev",prop.learning=0.5)  ### AMM - Should this be hardcoded with mdev? As is specific to housing data
{
#Functions that go into penalized fitting method  
  aim2.list <- list(eval=aim2.eval, split=aim2.split, init=aim2.init, summary=aim2.summary, text=aim2.text)
  set.seed(seed)
  n <- nrow(dat)
#Identify outcome variable
    which.outcome <- which(colnames(dat)==outvar)
    if(length(which.outcome)==0) stop(paste("No variable found labeled as",outvar))
  colnames(dat)[which.outcome] <- "outvar.aim2"
#Make training and test data
  ntrain <- round(prop.learning*n)
  ntest <- n-ntrain
  samp <- sample(1:n,n,replace=FALSE)
  wtrain <- sort(samp[1:ntrain])
  wtest <- sort(samp[(ntrain+1):n])
  training.dat <- dat[wtrain,]
  test.dat <- dat[wtest,]
#The lambdas chosen by Rob's method  
  fit.rf.training <- randomForest(outvar.aim2 ~ .,data = training.dat)
  predict.rf.test <- predict(fit.rf.training,newdata=test.dat,predict.all=TRUE)
  mean.test <- mean(test.dat$outvar.aim2)
  var.test <- var(test.dat$outvar.aim2)
  zbarhat <- mean(predict.rf.test$aggregate)
  var.z1s <-  apply(predict.rf.test$individual,1,var)
  alphas <- 1/var.z1s
  alphabar <- mean(alphas)
  lambda <- var.test/(ntest*alphabar*(mean.test-zbarhat)^2)
  lambdas <- seq(0,mult*lambda,length.out=n.grid)
  n.lambdas <- length(lambdas)
  error.lambdas <- rep(0,length(lambdas))
  fits <- vector("list",n.lambdas)
  predictions <- vector("list",n.lambdas)
  for(i in 1:n.lambdas)
    {
#  final.lambda <- find.lambda(dat=dat,lambdas=lambdas,list.object=aim2.list)
#      final.fit <- rpart(outvar.aim2 ~ .,data = test.dat,parms=list(lambda=final.lambda,yhat=predict.rf.test$aggregate,alpha=alphas),method=aim2.list)
      current.fit <- rpart(outvar.aim2 ~ .,data = test.dat,parms=list(lambda=lambdas[i],yhat=predict.rf.test$aggregate,alpha=alphas),method=aim2.list)
      predicted.fit <- predict(object=current.fit,newdata=test.dat)
      error.lambdas[i] <- sum((test.dat$outvar.aim2-predicted.fit)^2)
      fits[[i]] <- current.fit
      predictions[[i]] <- predictions
    }
  optimism <- corrected.lambda(dat=test.dat,lambdas=lambdas,list.object=aim2.list,model=fit.rf.training,predicted.values=predict.rf.test$aggregate,alphas=alphas,n.boot=10)
  Error.lambdas <- error.lambdas+optimism
#  list(final.fit=final.fit,lambdas=lambdas,final.lambda=final.lambda)
  list(lambdas=lambdas,Error.lambdas=Error.lambdas,error.lambdas=error.lambdas,optimism=optimism,fits=fits,predictions=predictions,current.fit=current.fit,predicted.fit=predicted.fit,test.dat=test.dat)
}
#final.fit is rpart tree chosen by optimal lambda
#lambdas are the values of lambda from grid search
#final.lambda is the optimal lambda chosen by 3-fold cross-validation

housing.data <- as.data.frame(matrix(scan("housing.data"),nrow=506,byrow=TRUE))
colnames(housing.data) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratiob","b","lstat","mdev")
temp <- aim2(dat=housing.data,nreps=1,n.grid=20,mult=2,seed=12345,outvar="mdev")

write(temp$lambdas,"temp_lambdas.txt",ncol=1)
write(temp$Error.lambdas,"temp_Error_lambdas.txt",ncol=1)

pdf("housing_error.pdf")

plot(temp$lambdas,temp$Error.lambdas,xlab="lambda",ylab="Optimism corrected error",main="Optmism corrected error vs lambda for housing data")

dev.off()
