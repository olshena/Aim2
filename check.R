housing.data <- as.data.frame(matrix(scan("housing.data"),nrow=506,byrow=TRUE))
colnames(housing.data) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratiob","b","lstat","mdev")
temp <- aim2(dat=housing.data,nreps=1,n.grid=20,mult=2,seed=12345,outvar="mdev")

write(temp$lambdas,"temp_lambdas.txt",ncol=1)
write(temp$Error.lambdas,"temp_Error_lambdas.txt",ncol=1)

pdf("housing_error.pdf")

plot(temp$lambdas,temp$Error.lambdas,xlab="lambda",ylab="Optimism corrected error",main="Optmism corrected error vs lambda for housing data")

dev.off()
