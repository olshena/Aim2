#Look at housing data from UCI repository, downloaded 11/10/15

library(rpart)
library(randomForest)

source("code.R")

print(sessionInfo())

housing.data <- as.data.frame(matrix(scan("housing.data"),nrow=506,byrow=TRUE))
colnames(housing.data) <- c("CRIM","ZN","INDUS","CHAS","NOX","RM","AGE","DIS","RAD","TAX","PTRATIOB","B","LSTAT","MEDV")
colnames(housing.data) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratiob","b","lstat","mdev")

set.seed(12345)

which.training <- sort(sample(1:506,253,replace=FALSE))
which.test <- (1:506)[-which.training]

housing.data.training <- housing.data[which.training,]
housing.data.test <- housing.data[which.test,]

fit.rpart.training <- rpart(mdev ~ .,data = housing.data.training)
fit.rpart.test <- rpart(mdev ~ .,data = housing.data.test)
predict.rpart.test <- predict(fit.rpart.training,newdata=housing.data.test)

fit.rf.training <- randomForest(mdev ~ .,data = housing.data.training)
fit.rf.test <- randomForest(mdev ~ .,data = housing.data.test)
predict.rf.test <- predict(fit.rf.training,newdata=housing.data.test,predict.all=TRUE)

residuals.rpart.test <- housing.data.test$mdev-predict.rpart.test
residuals.rf.test <- housing.data.test$mdev-predict.rf.test$aggregate

squared.residuals.rpart.test <- sum(residuals.rpart.test^2)
squared.residuals.rf.test <- sum(residuals.rf.test^2)

abs.residuals.rpart.test <- sum(abs(residuals.rpart.test))
abs.residuals.rf.test <- sum(abs(residuals.rf.test))

var.z1s <-  apply(predict.rf.test$individual,1,var)
alphas <- 1/var.z1s

mean.test <- mean(housing.data.test$mdev)
var.test <- var(housing.data.test$mdev)
n <- nrow(housing.data.test)
zbarhat <- mean(predict.rf.test$aggregate)
alphabar <- mean(alphas)

lambda <- var.test/(n*alphabar*(mean.test-zbarhat)^2)
lambda0 <- 0

#aim2.list <- list(eval=aim2.eval, split=aim2.split, init=aim2.init, summary=aim2.summary, text=aim2.text)
#aim2.fit <- rpart(mdev ~ .,data = housing.data.test,parms=list(lambda=lambda,yhat=predict.rf.test$aggregate,alpha=alphas),method=aim2.list)

source("code.R")
aim2.list0 <- list(eval=aim2.eval, split=aim2.split, init=aim2.init, summary=aim2.summary, text=aim2.text)
aim2.fit0 <- rpart(mdev ~ .,data = housing.data.test,parms=list(lambda=lambda0,yhat=predict.rf.test$aggregate,alpha=alphas),method=aim2.list0)


which.greater <- which(housing.data.test[,13]>=5.445)
dats <- housing.data.test[which.greater,14]
mean.dats <- mean(dats)
res.dats <- sum((dats-mean.dats)^2)


order.data <- c(146,32,34,173,109,150,107,95,43,    237,246, 251)




