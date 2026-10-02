library(tidyverse)

my_glm <- function(Y,X,V,linkinv,dlink){
  n = length(Y)
  p = ncol(X)
  max_iter = 100
  tol = 10**(-3)
  
  beta = rep(0, p)
  for (iter in 1:max_iter) {
  linpred = X%*%beta
  mu = linkinv(linpred)
  M = dlink(mu)
  Varmean = V(mu)
  w = 1/(Varmean * (M**2))
  w = diag(as.vector(w))
  z = linpred + (Y - mu)*M
  nextbeta = as.vector(solve(t(X)%*%w%*%X)%*%t(X)%*%w%*%z)
  
  if (sqrt(sum((beta-nextbeta)**2))<tol){
    return(nextbeta)
  }
  beta = nextbeta
  }
  beta_hat = beta
  return(beta_hat)
}

n <- 100
betav <- rep(0.5,3)
Xv <- cbind(rep(1,n),matrix(rnorm(2*n),ncol=2))
muv <- exp(Xv %*% betav)
Yv <- rpois(n,muv)

V = identity
linkinv = exp
dlink = function(x){1/x}

my_glm(Yv,Xv,V,linkinv,dlink)
