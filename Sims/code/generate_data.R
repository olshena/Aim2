
library("R.utils")
library("Rlab")
setwd("~/Respository/Aim2/Sims")
source("code/sim_models.R")

model<-cmdArg(model="sim1")
tag <-cmdArg(tag="5000")
n.training <- cmdArg(n.training=5000L)
n.test <- cmdArg(n.test=10000L)
n.simulations <- cmdArg(n.simulations=500L)
    
print(paste("model","=",model))
print(paste("tag","=",tag))
print(paste("n.training","=",n.training))
print(paste("n.test","=",n.test))      
print(paste("n.simulations","=",n.simulations))
      
runmodel <- get(model)

for(i in 1:n.simulations)
    {
        current.model <- runmodel(n.training=n.training,n.test=n.test,seed=i)
        
        write.table(current.model$training.data,paste("data/",model,"_",tag,"_","training_data_",i,".txt",sep=""),sep="\t",row.names=FALSE,col.names=FALSE,quote=FALSE)
        write(current.model$training.y,paste("data/",model,"_",tag,"_","training_y_",i,".txt",sep=""),ncol=1)
        write(current.model$training.truth,paste("data/",model,"_",tag,"_","training_truth_",i,".txt",sep=""),ncol=1)

        write.table(current.model$test.data,paste("data/",model,"_",tag,"_","test_data_",i,".txt",sep=""),sep="\t",row.names=FALSE,col.names=FALSE,quote=FALSE)
        write(current.model$test.y,paste("data/",model,"_",tag,"_","test_y_",i,".txt",sep=""),ncol=1)
        write(current.model$test.truth,paste("data/",model,"_",tag,"_","test_truth_",i,".txt",sep=""),ncol=1)
}

