
library("R.utils")
library("listenv")
library("future")

source("/Users/annettemolinaro/Repository/Aim2/IntegratingCodeAndText_postAMM.R")


model<-cmdArg(model="sim1")
tag <-cmdArg(tag="500")
n.training <- cmdArg(n.training=500L)
n.test <- cmdArg(n.test=1000L)
n.simulations <- cmdArg(n.simulations=500L)
name <- paste("data/",model,"_",tag,"_",sep="")
CompositeDC <- paste("CompositeDC/October092017/",model,"_",tag,"_",sep="")
  
for (idx in 1:n.simulations)
{
  set.seed(idx)
  training.data <- matrix(scan(paste(name,"training_data_",idx,".txt",sep="")),nrow=n.training,byrow=TRUE)
  test.data <- matrix(scan(paste(name,"test_data_",idx,".txt",sep="")),nrow=n.test,byrow=TRUE)
  training.y <- scan(paste(name,"training_y_",idx,".txt",sep=""))
  test.y <- scan(paste(name,"test_y_",idx,".txt",sep=""))	
  training.truth <- scan(paste(name,"training_truth_",idx,".txt",sep=""))
  test.truth <- scan(paste(name,"test_truth_",idx,".txt",sep=""))

  training.data.all<-data.frame(cbind(training.data,training.y))
  dimnames(training.data.all)[[2]]<-c("X1","X2","X3","X4","X5","X6","X7","X8","X9","training.y")
  dimnames(test.data)[[2]]<-c("X1","X2","X3","X4","X5","X6","X7","X8","X9")
  test.data<-as.data.frame(test.data)

  system.time(Model<-composite.rpart(dat=training.data.all,n.grid=100,uplim=20,outvar="training.y"))
  lambda.num<-which(Model$error.lambdas==min(Model$error.lambdas))
  MinErrorLambda<-Model$lambdas[lambda.num]
  PredictedValuesCurrent<-predict(object=Model$fits[[lambda.num]],newdata=test.data)
  ErrorToTruthCurrent<-(sum(( PredictedValuesCurrent - as.matrix(test.truth))^2))/n.test
  
  write(MinErrorLambda,paste(CompositeDC,"CompositeDC_Train500_uplim20_MinErrorLambda_",idx,".txt",sep=""),ncol=1)
  write(PredictedValuesCurrent,paste(CompositeDC,"CompositeDC_Train500_uplim20_PredictedValuesCurrent_",idx,".txt",sep=""),ncol=1)
  write(ErrorToTruthCurrent,paste(CompositeDC,"CompositeDC_Train500_uplim20_ErrorToTruthCurrent_",idx,".txt",sep=""),ncol=1)        
}


