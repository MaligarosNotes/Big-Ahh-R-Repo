library(tidyverse)
install.packages('IntCens')
# library(Rcpp)
# library(IntCens)

DATA =ISLR::Weekly
DATA[, "Direction"] <- ifelse(DATA[, "Direction"] == "Up", 1, 0)
DATA

X = DATA[,1:7]
X

n <- nrow(DATA)
n

P = rbinom(n,1,0.7)
A <- 2 * P - 1
A

R = DATA[,'Today'] * A
R

model = glm(P~Year+Lag1+Lag2+Lag3+Lag4+Lag5+Volume+Today+Direction,data = DATA,family=binomial(link = logit))
summary(model)

pi_hat(DATA[1,],-1)

pi_hat <- function(x,a) {
  ep = 0.01
  e_hat = predict(model, x)
  pi = a*e_hat + (1-a)/2
  if (pi > 1-ep){
    return(1-ep)
  }
  else if (pi < ep){
    return(ep)
  }
  else {
    return(pi)
  }
}
