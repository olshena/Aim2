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

aim2.split <- function(y, wt, x, parms, continuous)
  {
    n <- length(y)
    y1 <- y[,1]
    yhat <- y[,2]
    alpha <- y[,3]
    if (continuous)
      {
        goodness <- direction <- double(n-1) #Allocate 0 vector
        y.cumsum <- cumsum(y1)
        y.left <- y.cumsum[-n]
        y.right <- y.cumsum[n]-y.left
        yhat.cumsum <- cumsum(yhat*alpha)
        yhat.left <- yhat.cumsum[-n]
        yhat.right <- yhat.cumsum[n]-yhat.left
        alpha.cumsum <- cumsum(alpha)
        alpha.left <- alpha.cumsum[-n]
        alpha.right <- alpha.cumsum[n]-alpha.left
        for(i in 1:(n-1))
          {
            zbar.left <- y.left[i]/i
            zbar.right <- y.right[i]/(n-i)
            zbarhat.left <- yhat.left[i]/alpha.left[i]
            zbarhat.right <- yhat.right[i]/alpha.right[i]
            alphabar.left <- alpha.left/i
            alphabar.right <- alpha.right/(n-i)
            r.left <- 1/(1+lambda*alphabar.left)
            r.right <- 1/(1+lambda*alphabar.right)
            chat.left <- r.left*zbar.left+(1-r.left)*zbarhat.left
            chat.right <- r.right*zbar.right+(1-r.right)*zbarhat.right
            goodness[i] <- sum((y1[1:i]-chat.left)^2 + lambda*alpha[1:i]*(yhat[1:i]-chat.left)^2) +
              sum((y1[(i+1):n]-chat.right)^2 + lambda*alpha[(i+1):n]*(yhat[(i+1):n]-chat.right)^2)
            if(zbar.left>zbar.right) direction[i] <- 1
            else direction[i] <- (-1)
          }
      }
    list(goodness=goodness, direction=direction)    
  }

    
