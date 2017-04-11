housing.data <- as.data.frame(matrix(scan("housing.data"),nrow=506,byrow=TRUE))
colnames(housing.data) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratiob","b","lstat","mdev")

#Regular check

temp <- aim2(dat=housing.data,nreps=1,n.grid=20,mult=2,seed=12345,outvar="mdev")

write(temp$lambdas,"temp_lambdas.txt",ncol=1)
write(temp$Error.lambdas,"temp_Error_lambdas.txt",ncol=1)

pdf("housing_error.pdf")

plot(temp$lambdas,temp$Error.lambdas,xlab="lambda",ylab="Optimism corrected error",main="Optmism corrected error vs lambda for housing data")

dev.off()

#Check with finer grid search

temp2 <- aim2(dat=housing.data,nreps=1,n.grid=20,mult=2,seed=12345,outvar="mdev",finer.grid=30)

write(temp2$lambdas,"temp2_lambdas.txt",ncol=1)
write(temp2$Error.lambdas,"temp2_Error_lambdas.txt",ncol=1)

pdf("housing_error2.pdf")

plot(temp2$lambdas,temp2$Error.lambdas,xlab="lambda",ylab="Optimism corrected error",main="Optmism corrected error vs lambda for housing data")

dev.off()

