# Assignment analysis helpers


# load_assignment_data <- function(path = "data/raw/students.csv") {
#   if (!file.exists(path)) {
#     stop("Data file not found: ", path)
#   }
#   read.csv(path)
# }
# 
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
