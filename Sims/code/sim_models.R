sim1 <- function(n.training,n.test,seed)
  {

    pi.var<-0.5
    x2.pi.var<-0.2 #when X1 == 0
    total.indp.bernoulli <-8
    total.variables <- 9

    training.data <- matrix(NA_integer_,n.training,9)
    training.y <- array(NA_real_,n.training)
    training.truth <- array(NA_integer_,n.training)
    
    test.data <- matrix(NA_integer_,n.test,9)
    test.y <- array(NA_real_,n.test)
    test.truth <- array(NA_integer_,n.test)

    set.seed(seed)

    for(i in 1:n.training)
        {
            indep.bernoulli.vector<- rbern(total.indp.bernoulli,pi.var)
            if(indep.bernoulli.vector[1]==0){
                X2<-rbern(1,x2.pi.var)
            }else{
                X2<-rbern(1,(1-x2.pi.var))
            }
            training.data[i,1]<- indep.bernoulli.vector[1]
            training.data[i,2]<-X2
            training.data[i,3:total.variables]<-indep.bernoulli.vector[2:total.indp.bernoulli]
            if( (!training.data[i,1]) & training.data[i,2]) {
                training.y[i]<- 5*(training.data[i,1] | training.data[i,2])+rnorm(1,0,3)
            }else{
                training.y[i]<-5*(training.data[i,1] | training.data[i,2])+rnorm(1,0,1)
            }
            training.truth[i]<-5*(training.data[i,1] | training.data[i,2])
        }

    for(i in 1:n.test)
      {
          indep.bernoulli.vector<- rbern(total.indp.bernoulli,pi.var)
          if(indep.bernoulli.vector[1]==0){
              X2<-rbern(1,x2.pi.var)
          }else{
              X2<-rbern(1,(1-x2.pi.var))
          }
          test.data[i,1]<- indep.bernoulli.vector[1]
          test.data[i,2]<-X2
          test.data[i,3:total.variables]<-indep.bernoulli.vector[2:total.indp.bernoulli]
      
          if( (!test.data[i,1]) & test.data[i,2]) {
              test.y[i]<- 5*(test.data[i,1] | test.data[i,2])+rnorm(1,0,3)
          }else{
              test.y[i]<-5*(test.data[i,1] | test.data[i,2])+rnorm(1,0,1)
          }
          test.truth[i]<-5*(test.data[i,1] | test.data[i,2])
      }

    list(training.data=training.data,training.y=training.y,training.truth=training.truth,test.data=test.data,test.y=test.y,test.truth=test.truth)

  }

sim1b <- function(n.training,n.test,seed)
  {
    
    pi.var<-0.5
    x2.pi.var<-0.2 #when X1 == 0
    total.indp.bernoulli <-8
    total.variables <- 9
    
    training.data <- matrix(NA_integer_,n.training,9)
    training.y <- array(NA_real_,n.training)
    training.truth <- array(NA_integer_,n.training)
    
    test.data <- matrix(NA_integer_,n.test,9)
    test.y <- array(NA_real_,n.test)
    test.truth <- array(NA_integer_,n.test)
    
    set.seed(seed)
    
    for(i in 1:n.training)
      {
        indep.bernoulli.vector<- rbern(total.indp.bernoulli,pi.var)
        if(indep.bernoulli.vector[1]==0){
          X2<-rbern(1,x2.pi.var)
        }else{
          X2<-rbern(1,(1-x2.pi.var))
        }
        training.data[i,1]<- indep.bernoulli.vector[1]
        training.data[i,2]<-X2
        training.data[i,3:total.variables]<-indep.bernoulli.vector[2:total.indp.bernoulli]
 	
        if( (!training.data[i,1]) & training.data[i,2]) {
          training.y[i]<- 5*(training.data[i,1] & training.data[i,2])+rnorm(1,0,3)
        }else{
          training.y[i]<-5*(training.data[i,1] & training.data[i,2])+rnorm(1,0,1)
        }
        training.truth[i]<-5*(training.data[i,1] & training.data[i,2])
      }
    
    for(i in 1:n.test)
      {
        indep.bernoulli.vector<- rbern(total.indp.bernoulli,pi.var)
        if(indep.bernoulli.vector[1]==0){
          X2<-rbern(1,x2.pi.var)
        }else{
          X2<-rbern(1,(1-x2.pi.var))
        }
        test.data[i,1]<- indep.bernoulli.vector[1]
        test.data[i,2]<-X2
        test.data[i,3:total.variables]<-indep.bernoulli.vector[2:total.indp.bernoulli]
 	
        if( (!test.data[i,1]) & test.data[i,2]) {
          test.y[i]<- 5*(test.data[i,1] & test.data[i,2])+rnorm(1,0,3)
        }else{
          test.y[i]<-5*(test.data[i,1] & test.data[i,2])+rnorm(1,0,1)
        }
        test.truth[i]<-5*(test.data[i,1] & test.data[i,2])
      }

    list(training.data=training.data,training.y=training.y,training.truth=training.truth,test.data=test.data,test.y=test.y,test.truth=test.truth)
    
  }

sim2 <- function(n.training,n.test,seed)
  {

    training.data <- matrix(NA_real_,n.training,9)
    training.y <- array(NA_real_,n.training)
    training.truth <- array(NA_integer_,n.training)
    
    test.data <- matrix(NA_real_,n.test,9)
    test.y <- array(NA_real_,n.test)
    test.truth <- array(NA_integer_,n.test)

    set.seed(seed)

    for(i in 1:n.training)
      {
        training.data[i,]<-runif(9)
  
        if((training.data[i,1]>0.6)&(training.data[i,2]<=0.15))
          {
            training.y[i]<- 5*((training.data[i,1]<=0.6) | (training.data[i,2]<=0.15))+rnorm(1,0,0.25)
          }
        else
          {
            training.y[i]<- 5*((training.data[i,1]<=0.6) | (training.data[i,2]<=0.15))+rnorm(1,0,1)
          }
        
        training.truth[i]<-5*((training.data[i,1]<=0.6) | (training.data[i,2]<=0.15))
      }

    for(i in 1:n.test)
      {
       test.data[i,]<-runif(9)
  
       if((test.data[i,1]>0.6)&(test.data[i,2]<=0.15))
         {
           test.y[i]<- 5*((test.data[i,1]<=0.6) | (test.data[i,2]<=0.15))+rnorm(1,0,0.25)
         }
       else
         {
           test.y[i]<- 5*((test.data[i,1]<=0.6) | (test.data[i,2]<=0.15))+rnorm(1,0,1)
         }
       
       test.truth[i]<-5*((test.data[i,1]<=0.6) | (test.data[i,2]<=0.15))
     }

    list(training.data=training.data,training.y=training.y,training.truth=training.truth,test.data=test.data,test.y=test.y,test.truth=test.truth)

  }

sim2b <- function(n.training,n.test,seed)
  {

    training.data <- matrix(NA_real_,n.training,9)
    training.y <- array(NA_real_,n.training)
    training.truth <- array(NA_integer_,n.training)
    
    test.data <- matrix(NA_real_,n.test,9)
    test.y <- array(NA_real_,n.test)
    test.truth <- array(NA_integer_,n.test)

    set.seed(seed)

    for(i in 1:n.training)
      {
        test.data[i,]<-runif(9)
  
        if((test.data[i,1]>0.6)&(test.data[i,2]<=0.15))
          {
            test.y[i]<- 5*((test.data[i,1]<=0.6) | (test.data[i,2]<=0.15))+rnorm(1,0,0.25)
          }
        else
          {
            test.y[i]<- 5*((test.data[i,1]<=0.6) | (test.data[i,2]<=0.15))+rnorm(1,0,1)
          }
        
        test.truth[i]<-5*((test.data[i,1]<=0.6) | (test.data[i,2]<=0.15))
     }

    for(i in 1:n.test)
      {
        test.data[i,]<-runif(9)
        
        if((test.data[i,1]>0.6)&(test.data[i,2]<=0.15))
          {
            test.y[i]<- 5*((test.data[i,1]<=0.6) | (test.data[i,2]<=0.15))+rnorm(1,0,0.25)
          }
        else
          {
            test.y[i]<- 5*((test.data[i,1]<=0.6) | (test.data[i,2]<=0.15))+rnorm(1,0,1)
          }
        
        test.truth[i]<-5*((test.data[i,1]<=0.6) | (test.data[i,2]<=0.15))
        
      }
    list(training.data=training.data,training.y=training.y,training.truth=training.truth,test.data=test.data,test.y=test.y,test.truth=test.truth)
    
  }

friedman1 <- function(n.training,n.test,seed)
  {

    training.data <- matrix(NA_real_,n.training,10)
    training.y <- array(NA_real_,n.training)
    training.truth <- array(NA_real_,n.training)
    
    test.data <- matrix(NA_real_,n.test,10)
    test.y <- array(NA_real_,n.test)
    test.truth <- array(NA_real_,n.test)

    set.seed(seed)

    for(i in 1:n.training)
      {

        training.data[i,]<-runif(10)
        training.truth[i]<-10*sin(pi*training.data[i,1]*training.data[i,2])+20*(training.data[i,3]-0.5)^2+10*training.data[i,4]+5*training.data[i,5]
        training.y[i]<- training.truth[i]+rnorm(1)
     }

    for(i in 1:n.test)
      {
        test.data[i,]<-runif(10)
        test.truth[i]<-10*sin(pi*test.data[i,1]*test.data[i,2])+20*(test.data[i,3]-0.5)^2+10*test.data[i,4]+5*test.data[i,5]
        test.y[i]<- test.truth[i]+rnorm(1)
      }
    list(training.data=training.data,training.y=training.y,training.truth=training.truth,test.data=test.data,test.y=test.y,test.truth=test.truth)
    
  }

friedman2 <- function(n.training,n.test,seed)
  {

    training.data <- matrix(NA_real_,n.training,12)
    training.y <- array(NA_real_,n.training)
    training.truth <- array(NA_real_,n.training)
    
    test.data <- matrix(NA_real_,n.test,12)
    test.y <- array(NA_real_,n.test)
    test.truth <- array(NA_real_,n.test)

    set.seed(seed)

    for(i in 1:n.training)
      {
        training.data[i,]<-c(runif(1,0,100),2*pi*runif(1,20,280),runif(1,0,1),runif(1,1,11),
                             runif(1,0,100),2*pi*runif(1,20,280),runif(1,0,1),runif(1,1,11),
                             runif(1,0,100),2*pi*runif(1,20,280),runif(1,0,1),runif(1,1,11))
        training.truth[i]<-sqrt(training.data[i,1]^2+(training.data[i,2]*training.data[i,3]-(1/(training.data[i,2]*training.data[i,4])))^2)
        training.y[i]<- training.truth[i]+rnorm(1,0,379.110)
     }

    for(i in 1:n.test)
      {
        test.data[i,]<-c(runif(1,0,100),2*pi*runif(1,20,280),runif(1,0,1),runif(1,1,11),
                         runif(1,0,100),2*pi*runif(1,20,280),runif(1,0,1),runif(1,1,11),
                         runif(1,0,100),2*pi*runif(1,20,280),runif(1,0,1),runif(1,1,11))
        test.truth[i]<- sqrt(test.data[i,1]^2+(test.data[i,2]*test.data[i,3]-(1/(test.data[i,2]*test.data[i,4])))^2)
        test.y[i]<- test.truth[i]+rnorm(1,0,379.110)
      }
    list(training.data=training.data,training.y=training.y,training.truth=training.truth,test.data=test.data,test.y=test.y,test.truth=test.truth)
    
  }

friedman3 <- function(n.training,n.test,seed)
  {

    training.data <- matrix(NA_real_,n.training,12)
    training.y <- array(NA_real_,n.training)
    training.truth <- array(NA_real_,n.training)
    
    test.data <- matrix(NA_real_,n.test,12)
    test.y <- array(NA_real_,n.test)
    test.truth <- array(NA_real_,n.test)

    set.seed(seed)

    for(i in 1:n.training)
      {
        training.data[i,]<-c(runif(1,0,100),2*pi*runif(1,20,280),runif(1,0,1),runif(1,1,11),
                             runif(1,0,100),2*pi*runif(1,20,280),runif(1,0,1),runif(1,1,11),
                             runif(1,0,100),2*pi*runif(1,20,280),runif(1,0,1),runif(1,1,11))
        training.truth[i]<- atan((training.data[i,2]*training.data[i,3]-(1/(training.data[i,2]*training.data[i,4])))/training.data[i,1])
        training.y[i]<- training.truth[i]+rnorm(1,0,0.316)
      }

    for(i in 1:n.test)
      {
        test.data[i,]<-c(runif(1,0,100),2*pi*runif(1,20,280),runif(1,0,1),runif(1,1,11),
                         runif(1,0,100),2*pi*runif(1,20,280),runif(1,0,1),runif(1,1,11),
                         runif(1,0,100),2*pi*runif(1,20,280),runif(1,0,1),runif(1,1,11))
        test.truth[i]<- atan((test.data[i,2]*test.data[i,3]-(1/(test.data[i,2]*test.data[i,4])))/test.data[i,1])
        test.y[i]<- test.truth[i]+rnorm(1,0,0.316)
      }
    list(training.data=training.data,training.y=training.y,training.truth=training.truth,test.data=test.data,test.y=test.y,test.truth=test.truth)
    
  }





