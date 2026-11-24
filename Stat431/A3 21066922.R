library(tidyverse)

# Milagro Chen
# 21066922

my_qres <- function(Y,mu,distribution,m=NULL,k=NULL,make.plot=FALSE){
  if (distribution == 'binom'){
    a <- pbinom(round(Y*m)-1,m, mu)
    b <- pbinom(round(Y*m),m, mu)
  }
  
  if (distribution == 'pois'){
    a <- ppois(Y-1, mu)
    b <- ppois(Y, mu)
  }
  
  if (distribution == 'nbinom'){
    a <- pnbinom(q=Y-1,size=k,mu=mu)
    b <- pnbinom(q=Y,size=k, mu=mu)
  }
  
  u <- runif(length(mu), a, b)
  residuals <- qnorm(u)
  if (make.plot){
    plot(mu,residuals)
  }
  return(residuals)
}


# binomial
n <- 100
m <- sample(10:30,n,replace=TRUE)
x1 <- rnorm(n)
x2 <- rnorm(n)
mu <- plogis(-0.5 + 0.8*x1 - 0.5*x2)
Y <- rbinom(n,size=m,prob=mu)/m
my_qres(Y,mu,'binom',m,k=NULL,make.plot=TRUE)

# poisson
n <- 100
x1 <- rnorm(n)
x2 <- rnorm(n)
mu <- exp(0.5 + 0.3*x1 - 0.2*x2)
Y <- rpois(n,lambda=mu)
my_qres(Y,mu,'pois',m=NULL,k=NULL,make.plot=TRUE)

# negative binomial
n <- 100
k <- 3
x1 <- rnorm(n)
x2 <- rnorm(n)
mu <- exp(0.5 + 0.3*x1 - 0.2*x2)
Y <- rnbinom(n,size=k,mu=mu)
my_qres(Y,mu,'nbinom',m=NULL,k,make.plot=TRUE)
