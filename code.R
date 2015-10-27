#Implement interpretable trees code 10/17/2015

#y contains response, yhat from tree, and alpha where alpha=1/var(zhat)

aim2.init <- function(y, offset, parms, wt)
{
  if (!is.null(offset)) y[,1] <- y[,1]-offset
  if (!is.matrix(y)) stop("response must be a matrix")
  if (ncol(y)!=3) stop("response must be an n by 3 matrix")
  list(y=y, parms=parms, numy=3, numresp=1)
}

aim2.eval <- function(y, wt, parms)
{
  n <- length(y)
  lambda <- parms$lambda
  yhat <- y[,2]
  alphas <- y[,3]
  alphabar <- sum(alphas)/n
  y <- y[,1]
  r <- 1/(1+lambda*alphabar)
  zbar <- mean(y)
  zbarhat <- sum(yhat*alphas)/sum(alphas)
  chat <- r*zbar+(1-r)*zbarhat
  rss <- sum((y-chat)^2+lambda*alphas*(chat-yhat)^2)
  list(label=chat, deviance=rss)
}

#aim2.split <- function(y, wt, x, parms, continuous)
#  {
#    n <- length(y)
#    if (continuous)
#      {
#        goodness <- direction <- double(n-1) #Allocate 0 vector
#        y.cumsum <- cumsum(y[,1]*wt)
#        y.left <- y.cumsum[-n]
#        y.right <- y.cumsum[n]-y.left
#        yhat.cumsum <- cumsum(y[,2]*wt*y[,3])
#        yhat.left <- yhat.cumsum[-n]
#        yhat.right <- yhat.cumsum[n]-yhat.left
#        alpha.cumsum <- cumsum(y[,3])
#        alpha.left <- alpha.cumsum[-n]
#        alpha.right <- alpha.cumsum[n]-alpha.left
#        wt.cumsum <- cumsum(wt)
#        wt.left <- wt.cumsum[-n]
#        wt.right <- wt.cumsum[n]-wt.left
#        alpha.wt.cumsum <- cumsum(y[,3]*wt)
#        alpha.wt.left <- alpha.wt.cumsum[-n]
#        alpha.wt.right <- alpha.wt.cumsum[n]-alpha.wt.right
#        for(i in 1:(n-1))
#          {
#            zbar.left <- y.left[i]/wt.left[i]
#            zbar.right <- y.right[i]/wt.right[i]
#            zbarhat.left <- yhat.left[i]/alpha.wt.left[i]
#            zbarhat.right <- yhat.right[i]/alpha.wt.right[i]
#            alphabar.left <- alpha.left/i
#            alphabar.right <- alpha.right/(n-i)
#      }
#  }
#

    
