library(randomForest)
library(rpart)
library(rpart.plot)
library(logspline)

# R version of Matlab function 'repmat'. E.g., if given a vector 'X'
# of length 'k', then repmat(X,1,n) will give the k x n matrix where
# each matrix column is a copy of X.

repmat = function(X,m,n){
  if (is.vector(X)) X = matrix(X,length(X),1)
  mx = dim(X)[1]
  nx = dim(X)[2]
  return(matrix(t(matrix(X,mx,nx*n)),mx*m,nx*n,byrow=T))
}

# Use leave-one-out CV (LOO)
CVcorrected.lambda <- function(dat,lambdas,model,predicted.values,alphas)
  {
    n1 <- nrow(dat)
    p <- ncol(dat)
    n.lambdas <- length(lambdas)
    cilambda <- matrix(0,n1,n.lambdas)
   
    for(b in 1:n1)
      {
      
        # create new LOO dataset that deletes bth observation
        new.dat <- dat[-b,]
        
        # for bth LOO dataset, loop over lambda
        for(j in 1:n.lambdas)
          {          

			#derive modified response
            new.lambda <- lambdas[j]
      		ri <- 1/(1+alphas[-b]*new.lambda)
      		ci <- ri*new.dat$outvar.aim2 + (1-ri)*predicted.values[-b]

			# create modified LOO dataset
			use.dat = new.dat
			use.dat$outvar.aim2 = ci
			
            # fit model to modified LOO dataset
            final.fit <- rpart(outvar.aim2 ~ .,data = use.dat)
            
            #######################################################
			# prune this fit                               
            xgroup <- rep(1:10, length = nrow(use.dat))
            xfit <- xpred.rpart(final.fit,xgroup)
            
            # here -- calculates error using original bootstrap response 
            # (new.dat) and not use.dat (weighted response)
            xerror <- colMeans((xfit - new.dat$outvar.aim2)^2)
            
            ff = which(xerror==min(xerror))[1]
            min.CP<-final.fit$cptable[ff,1]
            final.fit.pruned<-prune(final.fit,cp=min.CP)
            #######################################################
             
             
            # calculate the predicted value for each subject in dat
            # from new LOO model built using dat[-b,]
            bigMhat <- predict(object=final.fit.pruned,newdata=dat)

			# get Zb - tree(-b,Wb) for the bth LOO at lambda = lambda[j]
			# and square the result
			diffpred = (dat$outvar.aim2 - bigMhat)[b] 
			cilambda[b,j] = diffpred^2
          }
      }
    return(apply(cilambda,2,sum))
  }


# parametric bootstrap - this has been modified from Adam's original version

corrected.lambda <- function(dat,lambdas,list.object,model,predicted.values,alphas,n.boot=10)
  {
    n1 <- nrow(dat)
    p <- ncol(dat)
    n.lambdas <- length(lambdas)
    cilambda <- matrix(0,n1,n.lambdas)
    
    # bootstrap the data - generate new 'Z's about \hat{Z} 
    
    # note: the code below is equivalent but faster than original bootstrap code
    # that used normal errors 
    # sigmahat <- sqrt(sum((dat$outvar.aim2-predicted.values)^2)/n1)     
    # boot.dat = matrix(rnorm(n.boot*n1,mean=predicted.values,sd=sigmahat),n1,n.boot)   
 	# boot.mean <- apply(boot.dat,1,mean)
 	# boot.residual = boot.dat-boot.mean
 	
 	# here, use a nonparametric density estimator for the errors 'Zi-\hat{Zi}'
 	# and then generate bootstrapped Z by adding random error to \hat{Zi}
 	yy = dat$outvar.aim2-predicted.values
 	fit = logspline(yy)
	booterr.dat = matrix(rlogspline(n.boot*n1,fit),n1,n.boot)
    boot.dat = repmat(predicted.values,1,n.boot)+booterr.dat
 	boot.mean <- repmat(as.vector(apply(boot.dat,1,mean)),1,n.boot)
 	boot.residual = boot.dat-boot.mean
	

    for(b in 1:n.boot)
      {

        new.dat <- dat
    	new.dat$outvar.aim2 <- boot.dat[,b]
        
        for(j in 1:n.lambdas)
          {
            new.lambda <- lambdas[j]
      		ri <- 1/(1+alphas*new.lambda)
      		ci <- ri*new.dat$outvar.aim2 + (1-ri)*predicted.values
			use.dat = new.dat
			use.dat$outvar.aim2 = ci
			
            # fit model to bootstrapped weighted response
            final.fit <- rpart(outvar.aim2 ~ .,data = use.dat)
            
            #######################################################
			# prune the bootstrap fit                               
            xgroup <- rep(1:10, length = nrow(use.dat))
            xfit <- xpred.rpart(final.fit,xgroup)
            
            # here -- calculates error using original bootstrap response 
            # (new.dat) and not use.dat (weighted response)
            xerror <- colMeans((xfit - new.dat$outvar.aim2)^2)

            ff = which(xerror==min(xerror))[1]
            min.CP<-final.fit$cptable[ff,1]
            final.fit.pruned<-prune(final.fit,cp=min.CP)
            #######################################################

            # calculate the predicted value for each subject in use.dat;
            # note that it does not matter whether use.dat or new.dat
            # is used here since the response is not used in deriving 
            # model prediction
            bigMhat <- predict(object=final.fit.pruned,newdata=use.dat)
            
             
            # for lambda=lambda[j], add bootstrap contributions
            # to covariance penalty from bth sample. The boot.residual
            # is the bootstrapped response, not weighted response. Also,
            # note that this is numerically equivalent to calculation that 
            # replaces 'bigMhat' with 'bigMhat-predicted.values'
           
            cilambda[,j] <- cilambda[,j]+bigMhat*boot.residual[,b]

          }
      }
    cilambda <- cilambda/(n.boot-1)
    return(2*apply(cilambda,2,sum))
  }



composite.rpart=function(dat,n.grid=20,mult=1,uplim=10,outvar="Y",prop.learning=0.5)
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

  # use learning set to derive RF predictions
  fit.rf.learning <- randomForest(outvar.aim2 ~ .,data = learning.dat,ntree=1000)
  predict.rf.evaluation <- predict(fit.rf.learning,newdata=evaluation.dat,predict.all=TRUE) 
  
  
  
  # mean, variance of Z's in evaluation set
  mean.evaluation <- mean(evaluation.dat$outvar.aim2) # $\mu_{Z_1}$
  var.evaluation <- var(evaluation.dat$outvar.aim2) # $\sigma^2_{Z_1}$
  
  # mean, variance of RF predictions for evaluation set
  zbarhat <- mean(predict.rf.evaluation$aggregate) # $ \bar{\hat{Z_1}}$ 
  var.z1s <-  apply(predict.rf.evaluation$individual,1,var) # $\sigma^2_{\hat{Z_1i}}$

  # Hard to know how to chooose. Need to think about objective. But sensible choice
  # in that high variance predictions would get lower weight
  alphas <- 1/var.z1s
  #alphas = rep(1,length(var.z1s))
  
  # "optimal" 'root node' lambda for specified alphas - not really relevant here since calculated
  # using a different objective. Could revisit this calculation.
  #alphabar <- mean(alphas) # \bar{\alpha}$
  #lambda <- var.evaluation/(neval*alphabar*(mean.evaluation-zbarhat)^2) 
  #print(paste("lambda =",lambda)) 									
  
  # grid of lambda values
  lambdas <- seq(0,uplim,length.out=n.grid)  # list of possible lambdas
  n.lambdas <- length(lambdas) #length of list

  error.lambdas <- rep(0,length(lambdas)) 
  errorU.lambdas <- rep(0,length(lambdas)) 
  fits <- vector("list",n.lambdas)
  predictions <- vector("list",n.lambdas)

  use.dat<-evaluation.dat

  for(j in 1:n.lambdas)
    {
    	
      new.lambda <- lambdas[j]
      new.denom <- (1+alphas*new.lambda)
      ri <- 1/new.denom
 	  
 	  # weighted response calculation
      ci <- ri*use.dat$outvar.aim2 + (1-ri)*predict.rf.evaluation$aggregate
  	  
 	  # build tree using dataset that employs weighted response
      new.use.dat <- use.dat
      new.use.dat$outvar.aim2 <- ci
      current.fit <- rpart(outvar.aim2 ~ .,data = new.use.dat)

	  # prune that tree using manual CV
      min.CP<-current.fit$cptable[which(current.fit$cptable[,4]==min(current.fit$cptable[,4])),1]
      current.fit.pruned<-prune(current.fit,cp=min.CP)
      
      # derive predicted outcomes for each observation in that dataset
      predicted.fit <- predict(object=current.fit.pruned, data=new.use.dat)
      
      # calculate "apparent error" using 'ci' as response
      error.lambdas[j] <- sum((new.use.dat$outvar.aim2-predicted.fit)^2)
 
      # calculate "apparent error" using actual response
      errorU.lambdas[j] <- sum((use.dat$outvar.aim2-predicted.fit)^2)
      
      
      fits[[j]] <- current.fit
      predictions[[j]] <- predicted.fit
    }


  CVError.lambdas <- CVcorrected.lambda(dat=evaluation.dat,lambdas=lambdas,
                               model=fit.rf.learning,
                               predicted.values=predict.rf.evaluation$aggregate,
                               alphas=alphas)


  Error.lambdas <- errorU.lambdas  
  optimism <- corrected.lambda(dat=evaluation.dat,lambdas=lambdas,
                               list.object=aim2.list,model=fit.rf.learning,
                               predicted.values=predict.rf.evaluation$aggregate,
                               alphas=alphas,n.boot=1000)
  Error.lambdas <- Error.lambdas+optimism
    

list(lambdas=lambdas,error.lambdas=error.lambdas,errorU.lambdas=errorU.lambdas,
	Error.lambdas=Error.lambdas,fits=fits,predictions=predictions,
	optimism = optimism, CVError.lambdas = CVError.lambdas )
}

################################################################################
######    RUN FOR HOUSING DATA 
################################################################################

housing.data <- as.data.frame(matrix(scan("housing.data"),nrow=506,byrow=TRUE))
colnames(housing.data) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad",
                            "tax","ptratiob","b","lstat","mdev")

set.seed(12345)			    
temp <- composite.rpart(dat=housing.data,n.grid=20,mult=1,uplim=20,outvar="mdev",prop.learning=.5)


     
doit=TRUE
if(doit) {
par(mfrow=c(2,2))
plot(temp$lambdas,temp$errorU.lambdas,xlab="lambda",ylab="apparent error",
     main="apparent errorU (orig response) vs. lambda")
plot(temp$lambdas,temp$Error.lambdas,xlab="lambda",ylab="Corrected error",
     main="op-corr apparent errorU vs. lambda")
plot(temp$lambdas,temp$error.lambdas,xlab="lambda",ylab="apparent error",
     main="apparent error vs. lambda")
plot(temp$lambdas,temp$CVError.lambdas,xlab="lambda",ylab="CV error",
     main="CV-based error vs. lambda")
}