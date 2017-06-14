
library("R.utils")
library("listenv")
library("future")

source("/Users/annettemolinaro/Respository/Aim2/IntegratingCodeAndText_postAMM.R")


model<-cmdArg(model="sim1")
tag <-cmdArg(tag="notag")
n.training <- cmdArg(n.training=250L)
n.test <- cmdArg(n.test=1000L)
n.simulations <- cmdArg(n.simulations=1000L)
leafytrees <- cmdArg(leafytrees=500L)
name <- paste("data/",model,"_",tag,"_",sep="")
rf <- paste("rf/",model,"_",tag,"_",sep="")
  
for (idx in 1:n.simulations)
{
  set.seed(idx)
  training.data <- matrix(scan(paste(name,"training_data_",idx,".txt",sep="")),nrow=n.training,byrow=TRUE)
  test.data <- matrix(scan(paste(name,"test_data_",idx,".txt",sep="")),nrow=n.test,byrow=TRUE)
  training.y <- scan(paste(name,"training_y_",idx,".txt",sep=""))
  test.y <- scan(paste(name,"test_y_",idx,".txt",sep=""))	
  training.truth <- scan(paste(name,"training_truth_",idx,".txt",sep=""))
  test.truth <- scan(paste(name,"test_truth_",idx,".txt",sep=""))

  system.time(Model<-randomForest(x=training.data,y=as.matrix(training.y),ntree = leafytrees))
  
  PredictedValuesCurrent<-predict(Model,test.data)
  VIMPCurrent<-importance(Model,scale=FALSE)
  ErrorToTruthCurrent<-(sum(( PredictedValuesCurrent - as.matrix(test.truth))^2))/n.test
  
  write(PredictedValuesCurrent,paste(rf,"rf_PredictedValuesCurrent_",idx,".txt",sep=""),ncol=1)
  write(VIMPCurrent,paste(rf,"rf_VIMPCurrent_",idx,".txt",sep=""),ncol=1)
  write(ErrorToTruthCurrent,paste(rf,"rf_ErrorToTruthCurrent_",idx,".txt",sep=""),ncol=1)        
}


