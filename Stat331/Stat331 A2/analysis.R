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
betahat_mat[2,]
#i)
hist(betahat_mat[2,])
#ii)
mean(betahat_mat[2,])
#iii)
sqrt(var(betahat_mat[2,]))
#iv)
# we know $\hat{beta_2}$ follows $N(beta_2,sigma^2(X^tX)^{-1}_{2,2})$
sqrt((solve(t(X)%*%X) * truesigma**2)[2,2])
#b)
(solve(t(X)%*%X) * truesigma**2)[3,4]
#36459.56 
cov(betahat_mat[3,],betahat_mat[4,])

#2.
#a)
ceocomp <- read.csv('~/Documents/GitHub/Big-Ahh-R-Repo/Stat331/Stat331 A2/data/raw/ceocomp.csv')
ceo_pos <- subset(ceocomp,ceocomp$PROF>0)
set.seed(21066922)
my.ceo_pos = ceo_pos[sample(nrow(ceo_pos),60),]   
my.ceo_pos 
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
fullmodel <- lm(comp ~ x1+x2+x3+x4+x5+x6+x7+x8+x9)
summary(fullmodel)

#b)
#keep in mind the hypothesis test is for betahat = 0 vector
r2_full <- summary(fullmodel)$r.squared
msreg <- r2_full/12
msres <- (1-r2_full)/47
1 - pf(msreg/msres,12,47)

#c)
#i) with beta hat x34 being -289, we say WITHOUT statistical significance that lawyers tend to make less than banking background
#ii) sales also makes less, but we're even LESS certain of this estimate.
#iii)
redmodel <- lm(comp ~ x1+x2+x4+x5+x6+x7+x8+x9)
r2_red  <- summary(redmodel)$r.squared

f_num <- (r2_full - r2_red)/4
f_den <- (1 - r2_full)/47
f_stat <- f_num/f_den
1 - pf(f_stat,4,47)
#iv)
anova(redmodel,fullmodel)
#d)
summary(redmodel)
#i) they are all correlated, as a long EXPER certainly implies longer TENURE and AGE
#ii) 
pairs(cbind(x1,x4,x5))
#iii)
agemodel <- lm(x1~x2+x4+x5+x6+x7+x8+x9)
summary(agemodel)
VIF <- 1/(1-0.2956)
#its ok
#e)
new_x <- data.frame(x1=48,x2=1,x4=14,x5=3,x6=3100,x7=2.1,x8=1.1,x9=172)
predict(redmodel,new_x,interval='prediction',level=.95)

