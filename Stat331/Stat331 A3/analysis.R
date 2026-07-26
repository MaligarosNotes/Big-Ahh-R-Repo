library(tidyverse)
#2.FROM A2 
#a)
ceocomp <- read.csv('~/Documents/GitHub/Big-Ahh-R-Repo/Stat331/Stat331 A2/data/raw/ceocomp.csv')
ceo_pos <- subset(ceocomp,ceocomp$PROF>0)
set.seed(21066922)
my.ceo_pos = ceo_pos[sample(nrow(ceo_pos),60),]   
my.ceo_pos 
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

#############################
#FROM HERE ONWARD IS A3 STUFF
#############################

#1.
plot(redmodel, which = 1)
#it looks roughly alright. some outliers, but thats about expected.
plot(redmodel, which = 2)
# the residuals follow a rough normal, which is expected, but with a slight right skew.

#2.
#a)
hist(comp)
#it has a heavy right tail, so using ln as a transform would send the outliers a lot closer to the rest of the data.
#b)
lncomp <- log(comp)
lnredmodel <- lm(lncomp ~ x1+x2+x4+x5+x6+x7+x8+x9)
#c)
summary(lnredmodel)
summary(redmodel)
#we see a very slight improvement
#d)
plot(lnredmodel, which = 1)
#ig its a lil better idk
plot(lnredmodel, which = 2)
#we have less skew, but still some fat tails.

#3.
#a)
plot(x6,lncomp)
# yea thats looks like ass
#b)
plot(log(x6),lncomp)
#its not rly much better bruh
#c)
lnmodel <- lm(lncomp ~ log(x6)+log(x7)+log(x8)+log(x9))
summary(lnmodel)              
#its honestly worse than the untransformed model, in terms of R^2 and adj R^2, but the p values individually are slightly better

#4.
plot(lnmodel, which = 1)
#ig its a lil better idk
plot(lnmodel, which = 2)
#ig its even closer to normal now. the tails are very slightly fatter than expected.

#5.
plot(fitted(lnmodel),rstudent(lnmodel))
qqnorm(rstudent(lnmodel)); qqline(rstudent(lnmodel))
#once again, it's ok, but not significantly different.

#6.
plot(hatvalues(lnmodel))
#rmb p = 4, n = 60, so we consider 'high-leverage' to be:
highlev = 2*(4+1)/60
abline(h = highlev)

#7.
hatvalues(lnmodel)[hatvalues(lnmodel)>0.4]
