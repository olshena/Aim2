#Look at housing data from UCI repository, downloaded 11/10/15

library(rpart)
library(randomForest)

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
predict.rpart.test <- predict(fit.rpart.training,newdata=housing.data.test)

fit.rf.training <- randomForest(mdev ~ .,data = housing.data.training)
predict.rf.test <- predict(fit.rf.training,newdata=housing.data.test)

residuals.rpart.test <- housing.data.test$mdev-predict.rpart.test
residuals.rf.test <- housing.data.test$mdev-predict.rf.test

squared.residuals.rpart.test <- sum(residuals.rpart.test^2)
squared.residuals.rf.test <- sum(residuals.rf.test^2)

abs.residuals.rpart.test <- sum(abs(residuals.rpart.test))
abs.residuals.rf.test <- sum(abs(residuals.rf.test))










