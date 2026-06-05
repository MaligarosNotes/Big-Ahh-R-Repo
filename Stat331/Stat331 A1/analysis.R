# Assignment analysis helpers


load_assignment_data <- function(path = "data/raw/students.csv") {
  if (!file.exists(path)) {
    stop("Data file not found: ", path)
  }
  read.csv(path)
}

A1_variates <- load_assignment_data('~/Documents/GitHub/Big-Ahh-R-Repo/Stat331/Stat331 A1/data/raw/A1_variates (1).txt')
# add_exam_average <- function(df, exam1 = "exam1", exam2 = "exam2") {
#   if (!all(c(exam1, exam2) %in% names(df))) {
#     stop("Required columns missing: ", exam1, ", ", exam2)
#   }
#   df$average <- (df[[exam1]] + df[[exam2]]) / 2
#   df
# }

library(data.table)

capm_s26 <- CAPM_S26

capm_s26$otex_Percent <- (CAPM_S26$OTEX - lag(CAPM_S26$OTEX, 1) ) / lag(CAPM_S26$OTEX, 1)
capm_s26$bb_Percent <- (CAPM_S26$BB - lag(CAPM_S26$BB, 1) ) / lag(CAPM_S26$BB, 1)
capm_s26$tsxc_Percent <- (CAPM_S26$TSXC - lag(CAPM_S26$TSXC, 1) ) / lag(CAPM_S26$TSXC, 1)
capm_s26$gvtb10_monthly <- (((CAPM_S26$GVTB10 / 100) + 1) ** (1/12)) -1

capm_s26 <- capm_s26[-1,-c(2,3,4,5)]

y <- capm_s26$bb_Percent - capm_s26$gvtb10_monthly
x <- capm_s26$tsxc_Percent - capm_s26$gvtb10_monthly

#a)
plot(x,y)
#b)
cor(x,y)
#c)
xbar <- mean(x)
ybar <- mean(y)

xvar <- sum((x - xbar)**2)
yvar <- sum((y - ybar)**2)

rho <- (sum((x - xbar)*(y - ybar)))/(sqrt(yvar*xvar))
#d)
slr <- lm(y ~ x)
summary(slr)$coefficients

#e)
#investing.com says 1.47, our model says 1.43
#f)
confint(slr)
t57_0975 <- qt(0.975, 57)
low <- summary(slr)$coefficients[2,1] - summary(slr)$coefficients[2,2] * t57_0975
high <- summary(slr)$coefficients[2,1] + summary(slr)$coefficients[2,2] * t57_0975
c(low,high)
#g)
alpha <- summary(slr)$coefficients[1,4]
alpha
#h)
  #i) alpha hat:
alphahat <- ybar - xbar* summary(slr)$coefficients[2,1] 
  #ii) beta hat:
betahat <- summary(slr)$coefficients[2,1]
  #iii) Standard Error of beta hat:
SEbetahat <- summary(slr)$coefficients[2,2]
  #iv) sigmahat:
sigmahat <- sqrt(sum((residuals(slr))^2)/(length(x)-2))
  #v) he p-value associated with H_0 : beta = 0
summary(slr)$coefficients[2,4]

#2. 
truebeta <- c(-200000, 45, -300, 10000, 150000)
truesigma <- 12000
A1_variates <- read_table('~/Documents/GitHub/Big-Ahh-R-Repo/Stat331/Stat331 A1/data/raw/A1_variates (1).txt')
n <- nrow(A1_variates)
n
X <- as.matrix(cbind(rep(1,n),A1_variates))
set.seed(21066922)

?rnorm
#a)
simulatedresponse <- (as.vector(X %*% truebeta) + rnorm(n,0,truesigma))
#this is the simulated data with the random seed set to my student number
simulatedresponse

#b)
y <- simulatedresponse
mlr <- lm(y~A1_variates$size+A1_variates$age+A1_variates$employees+A1_variates$col)
summary(mlr)

#c)
betashat <- solve(t(X)%*%X)%*%t(X) %*% y
#d)
confint(mlr)
