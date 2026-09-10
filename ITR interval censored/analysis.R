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
