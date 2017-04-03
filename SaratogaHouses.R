#Look at pneumonia data, 1728 obs, 15 predictors, 1 response $price in first column

library(rpart)
library(randomForest)

set.seed(12345)

dat <- read.csv("SaratogaHouses.csv",header=TRUE,row.names=1)

#> colnames(dat)
#[1] "price"           "lotSize"         "age"             "landValue"      
# [5] "livingArea"      "pctCollege"      "bedrooms"        "fireplaces"     
# [9] "bathrooms"       "rooms"           "heating"         "fuel"           
# [13] "sewer"           "waterfront"      "newConstruction" "centralAir"     

#Take square root of price

sdat <- dat
sdat$price <- sqrt(dat$price)

cor.price <- rep(NA,14)
for(i in 1:14) cor.price
    
fit <- rpart(price ~ .,data=sdat)
prune.fit <- prune(fit,cp=0.01)

fit2 <- randomForest(price ~ .,data=sdat)

n <- nrow(sdat)
samp <- sample(1:n,replace=FALSE)
n.train <- round(n/2)
n.test <- n-n.train
samp.train <- sort(samp[1:n.train])
samp.test <- sort(samp[(n.train+1):n])

sdat.train <- sdat[samp.train,]
sdat.test <- sdat[samp.test,]

fit.train <- rpart(price ~ .,data=sdat.train)
prune.train <- prune(fit.train,cp=0.01)
fit2.train <- randomForest(price ~ .,data=sdat.train)

predict.test <- predict(prune.train,newdata=sdat.test)
predict2.test <- predict(fit2.train,newdata=sdat.test)

res.test <- sdat.test$price-predict.test
res2.test <- sdat.test$price-predict2.test

par(mfrow=c(2,1))
hist(res.test)
hist(res2.test)

error.test <- sqrt(sum(res.test^2)/n.test)
error2.test <- sqrt(sum(res2.test^2)/n.test)

fit3 <- composite.rpart.thirds(dat=sdat.train,n.grid=20,mult=2,outvar="price")
predict3.test <- predict(fit3$current.fit.pruned,newdata=sdat.test)

res3.test <- sdat.test$price-predict3.test
error3.test <- sqrt(sum(res3.test^2)/n.test)
