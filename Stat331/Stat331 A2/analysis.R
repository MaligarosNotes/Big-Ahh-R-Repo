library(tidyverse)
#2.FROM A1 
truebeta <- c(-200000, 45, -300, 10000, 150000)
truesigma <- 12000
A1_variates <- read_table('~/Documents/GitHub/Big-Ahh-R-Repo/Stat331/Stat331 A1/data/raw/A1_variates (1).txt')
n <- nrow(A1_variates)
X <- as.matrix(cbind(rep(1,n),A1_variates))
set.seed(21066922)

#a)
simulatedresponse <- (as.vector(X %*% truebeta) + rnorm(n,0,truesigma))
#this is the simulated data with the random seed set to my student number
simulatedresponse

#b)
y <- simulatedresponse
size <- A1_variates$size
age <- A1_variates$age
employees <- A1_variates$employees
col <- A1_variates$col
mlr <- lm(y~size+age+employees+col)
summary(mlr)

#c)
betashat <- solve(t(X)%*%X)%*%t(X) %*% y
#d)
confint(mlr)
#############################
#FROM HERE ONWARD IS A2 STUFF
#############################

#1. 
betahat_mat = array(0,c(5,1000)) #creates an array to store sets of estimates
set.seed(21066922)
for (i in 1:1000){
  y<-X%*%truebeta+rnorm(24,0,12000) #X and Beta vector as defined in A1.
  A1sim.lm<-lm(y~size+age+employees+col)
  betahat_mat[,i]<-coef(A1sim.lm) #yields estimates for each iteration
}

#a)
betahat_mat[3,]
#i)
hist(betahat_mat[3,])
#ii)
xbar <- mean(betahat_mat[3,])
xbar
#iii)
var <- sum((betahat_mat[3,]-xbar)^2)/(length(betahat_mat[3,])-1)
sqrt(var)
#iv)
# we know $\hat{beta_2}$ follows $N(beta_2,sigma^2(X^tX)^{-1}_{3,3})$
solve(t(X)%*%X)


#2.
ceocomp
ceo_pos <- subset(ceocomp,ceocomp$PROF>0)
set.seed(21066922)
my.ceo_pos = ceo_pos[sample(nrow(ceo_pos),60),]   

?as.factor
comp <- my.ceo_pos$COMP
x1 <- my.ceo_pos$AGE
x2 <- my.ceo_pos$EDUCATN
x3 <- as.factor(my.ceo_pos$BACKGRD)
x4 <- my.ceo_pos$TENURE
x5 <- my.ceo_pos$EXPER
x6 <- my.ceo_pos$SALES
x7 <- my.ceo_pos$VAL
x8 <- my.ceo_pos$PCNTOWN
x9 <- my.ceo_pos$PROF
model <- lm(comp ~ x1+x2+x3+x4+x5+x6+x7+x8+x9)
model
