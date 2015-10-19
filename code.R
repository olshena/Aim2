#Implement interpretable trees code 10/17/2015

aim2.init <- function(y, offset, parms, wt)
{
  if (!is.null(offset)) y <- y-offset
  if (is.matrix(y)) stop("response must be a vector")
  list(y=y, parms=parms, numy=1, numresp=1)
}

aim2.eval <- function(y, wt, parms)
{
  n <- length(y)
  alphas <- parms$alphas
  lambda <- parms$lambda
  yhat <- parms$yhat
  alphabar <- sum(alphas)/n
  r <- 1/(1+lambda*alphabar)
  zbar <- sum(y*wt)/sum(wt) 
  zbarhat <- sum(yhat*wt*alphas)/sum(wt*alphas)
  chat <- r*zbar+(1-r)*zbarhat
  rss <- sum(wt*((y-chat)^2+lambda*alphas*(chat-yhat)^2))
  list(label=chat, deviance=rss)
}

aim2.split <- function(y, wt, x, parms, continuous)
  {
    n <- length(y)
    if (continuous)
      {
        goodness <- direction <- double(n-1) #Allocate 0 vector
        temp <- cumsum(y*wt)[-n]
        left.wt <- cumsum(wt)[-n]
        right.wt <- sum(wt)-left.wt
        alphas <- parms$alphas
        left.alphas <- cumsum(alphas)[-n]
        right.alphas <- sum(alphas)-left.alphas        
        yhat <- parms$yhat
        left.yhat <- cumsum(yhat)[-n]
        right.yhat <- sum(yhat)-left.yhat
        lmean <- temp/left.wt
        rmean <- temp/rt.wt
      }
  }
