#Implement interpretable trees code 10/17/2015

#y contains response, yhat from tree, and alpha where alpha=1/var(zhat)

aim2.init <- function(y, offset, parms, wt)
{
  if (!is.null(offset)) y[,1] <- y[,1]-offset
  list(y=cbind(y,parms$yhat,parms$alpha),parms=parms, numy=3, numresp=1)
}

aim2.eval <- function(y, wt, parms)
{
#  print("In eval")
  n <- length(y)
#  print("neval"); print(n)
  lambda <- parms$lambda
  yhat <- y[,2]
#  print("yeval");  print(y[1:10,1])
#  print("yhateval");  print(parms$yhat[1:10])  
  alphas <- y[,3]
#  print("alphainit");  print(parms$alpha[1:10])    
  alphabar <- sum(alphas)/n
  y <- y[,1]
  r <- 1/(1+lambda*alphabar)
  zbar <- mean(y)
  zbarhat <- sum(yhat*alphas)/sum(alphas)
  chat <- r*zbar+(1-r)*zbarhat
  print("chat"); print(chat)
  rss <- sum((y-chat)^2+lambda*alphas*(chat-yhat)^2)
  print("rss"); print(rss)
  list(label=chat, deviance=rss)
}

aim2.split <- function(y, wt, x, parms, continuous)
  {
    print("In split")
    print(y[1,])
    n <- length(y[,1])
    print(n)
    y1 <- y[,1]
    print("y1")
    print(y1[1:10])
    yhat <- y[,2]
    print("yhat")
    print(yhat[1:10])
    alpha <- y[,3]
    print("alpha")
    print(alpha[1:10])
    lambda <- parms$lambda
    print("lambda")
    print(lambda)
    if (continuous)
      {
        if(is.null(lambda)) compute.lambda #Placeholder until I figure out how to compute lambda
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
            print(i)
            zbar.left <- y.left[i]/i
            print("zbar.left")
            print(zbar.left)
            zbar.right <- y.right[i]/(n-i)
            print("zbar.right")
            print(zbar.right)
            zbarhat.left <- yhat.left[i]/alpha.left[i]
            zbarhat.right <- yhat.right[i]/alpha.right[i]
            alphabar.left <- alpha.left[i]/i
            alphabar.right <- alpha.right[i]/(n-i)
            r.left <- 1/(1+lambda*alphabar.left)
            r.right <- 1/(1+lambda*alphabar.right)
            chat.left <- r.left*zbar.left+(1-r.left)*zbarhat.left
            chat.right <- r.right*zbar.right+(1-r.right)*zbarhat.right
            print(paste("length of lambda",length(lambda)))
            print(paste("length of alphabar.left",length(alphabar.left)))
            print(paste("length of alphabar.right",length(alphabar.right)))
            print(paste("length of r.left",length(r.left)))
            print(paste("length of r.right",length(r.right)))
            goodness[i] <- sum((y1[1:i]-chat.left)^2 + lambda*alpha[1:i]*(yhat[1:i]-chat.left)^2) +
              sum((y1[(i+1):n]-chat.right)^2 + lambda*alpha[(i+1):n]*(yhat[(i+1):n]-chat.right)^2) #Do we need adjustment for missing values like in  vignette example?
            if(zbar.left>zbar.right) direction[i] <- 1
            else direction[i] <- (-1)
          }
      }
    list(goodness=goodness, direction=direction)    
  }

aim2.summary <- function(yval, dev, wt, ylevel, digits )
{
  paste(" mean=", format(signif(yval, digits)), ", MSE=" , format(signif(dev/wt, digits)), sep= '')
}

aim2.text <- function(yval, dev, wt, ylevel, digits, n, use.n )
{
  if(use.n) paste(formatg(yval,digits)," nn=", n,sep="")
  else paste(formatg(yval,digits))
}




