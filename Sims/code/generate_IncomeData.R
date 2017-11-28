
library("R.utils")
library("Rlab")
setwd("~/Respository/Aim2/Sims")
data<-read.csv("data/DataExamples/OnlineNewsPopularity.csv")

model<-cmdArg(model="Pop")
tag <-cmdArg(tag="5000")
n.training <- cmdArg(n.training=5000L)
n.test <- cmdArg(n.test=10000L)
n.simulations <- cmdArg(n.simulations=1000L)
    
print(paste("model","=",model))
print(paste("tag","=",tag))
print(paste("n.training","=",n.training))
print(paste("n.test","=",n.test))      
print(paste("n.simulations","=",n.simulations))
      


num.records<-nrow(data)
num.training<-5000
total.variables<-ncol(data)
for(i in 1:n.simulations)
    {
                	### Setting up the training set

  training.chosen <- sample(1:num.records,num.training,replace=FALSE)
  test.chosen <- (1:num.records)[-training.chosen]
    
  training.data <- data2[training.chosen,1:total.variables]
  training.truth <- data2[training.chosen,"shares"]
  training.y <- (training.truth)
  
	# setting up the test set

  test.data <- data2[test.chosen,1:total.variables]
  test.truth <- data2[test.chosen,"shares"]
  test.y <- test.truth
        write.table(training.data,paste("data/DataExamples/",model,"_",tag,"_","training_data_",i,".txt",sep=""),sep="\t",row.names=FALSE,col.names=FALSE,quote=FALSE)
        write(training.y,paste("data/DataExamples/",model,"_",tag,"_","training_y_",i,".txt",sep=""),ncol=1)
        write(training.truth,paste("data/DataExamples/",model,"_",tag,"_","training_truth_",i,".txt",sep=""),ncol=1)

        write.table(test.data,paste("data/DataExamples/",model,"_",tag,"_","test_data_",i,".txt",sep=""),sep="\t",row.names=FALSE,col.names=FALSE,quote=FALSE)
        write(test.y,paste("data/DataExamples/",model,"_",tag,"_","test_y_",i,".txt",sep=""),ncol=1)
        write(test.truth,paste("data/DataExamples/",model,"_",tag,"_","test_truth_",i,".txt",sep=""),ncol=1)
}

