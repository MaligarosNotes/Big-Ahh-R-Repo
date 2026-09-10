library(tidyverse)
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

model = glm(P~DATA[,1]+DATA[,2]+DATA[,3]+DATA[,4]+DATA[,5]+DATA[,6]+DATA[,7]+DATA[,8],family=binomial(link = logit))
summary(model)
pi_hat <- function(x,a) {
  if (a = 1){
    return predict(model, x)
  }
  
}