library(tidyverse)

# Milagro Chen
# 21066922

# I used the glm function given in the assignment pdf:
my_glm_2 <- function(Y,X,phi_vec,V,linkinv,dlink){
  # initializer
  beta_k <- rep(0, ncol(X))
  k_step <- 0
  stop_rule <- Inf
  while(stop_rule >= 10^(-3) & k_step < 100){
    # linear predictor
    eta <- X %*% beta_k
    # mean
    u <- linkinv(eta)
    M <- diag(as.vector(dlink(u)))
    W <- solve(M %*% diag(as.vector(phi_vec*V(u))) %*% M)
    z <- eta + M %*% (Y - u)
    beta_k1 <- solve(t(X) %*% W %*% X) %*% t(X) %*% W %*% z
    # update stopping rule
    stop_rule <- sqrt(sum((beta_k1 - beta_k)^2))
    # update beta and step
    beta_k <- beta_k1
    k_step <- k_step + 1
  }
  beta_hat <- beta_k
  return(beta_hat)
}


my_deviance <- function(Y,X1,X2,phi_vec,unit_deviance,V,linkinv,dlink){
  if (ncol(X1) >= ncol(X2)||qr(X1)$rank != ncol(X1) || qr(X2)$rank != ncol(X2) || qr(cbind(X2,X1))$rank != ncol(X2)){
    stop()
  }
  
  beta1_hat <- my_glm_2(Y,X1,phi_vec,V,linkinv,dlink)
  beta2_hat <- my_glm_2(Y,X2,phi_vec,V,linkinv,dlink)
  
  statistic = sum((unit_deviance(Y,linkinv(X1%*% beta1_hat))-unit_deviance(Y,linkinv(X2%*% beta2_hat)))/phi_vec)
  df = ncol(X2) - ncol(X1)
  p_value = pchisq(statistic, df, lower.tail = FALSE)
  return(c(statistic, p_value))
}

expit <- function(x){exp(x)/(1 + exp(x))}
n <- 100
m <- sample(10:30,n,replace=TRUE)
x1 <- rnorm(n)
x2 <- rnorm(n)
x3 <- rnorm(n)
mu <- expit(-0.5 + x1 - 0.5*x2)
Y <- rbinom(n,size=m,prob=mu)/m
phi_vec <- 1/m

# Binomial GLM functions
V <- function(mu){
mu*(1 - mu)
}
linkinv <- function(eta){
exp(eta)/(1 + exp(eta))
}
dlink <- function(mu){
1/(mu*(1 - mu))
}
unit_deviance <- function(y, mu) {
2*(ifelse(y == 0, 0, y * log(y / mu)) + ifelse(y == 1, 0, (1-y) * log((1-y) / (1-mu))))
}

# Example 1: non-nested models
X1 <- cbind(1,x1)
X2 <- cbind(1,x2,x3)


# Example 2: nested models
X1 <- cbind(1,x1)
X2 <- cbind(1,x1,x2)
my_deviance(Y,X1,X2,phi_vec,unit_deviance,V,linkinv,dlink)
