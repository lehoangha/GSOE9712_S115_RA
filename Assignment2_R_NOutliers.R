# This function is used for calculating the Con. Interval of Variance:
# The source of the function is from Washington Uni.
# https://www.stat.wisc.edu/~yandell/st571/R/append7.pdf
var.interval = function(data, conf.level = 0.95) {
  df = length(data) - 1
  chilower = qchisq((1 - conf.level)/2, df)
  chiupper = qchisq((1 - conf.level)/2, df, lower.tail = FALSE)
  v = var(data)
  cat('Confidence Interval for variance', 
      deparse(substitute(data)),
      '\n',
      df * v/chiupper, 
      df * v/chilower, '\n',      
      'Sample estimates:\n',
      'Variance of', deparse(substitute(data)), '\n',
      var(data))
}

testosterone <- read.csv("Assignment2_testosterone.csv")

# Create a new column named log_testosterone = log(testosterone) of exp(1)
testosterone$log_testosterone <- log(x = testosterone$testosterone, base = exp(1))

# Perform the F-test
var.test(x = testosterone[testosterone$treatment == "Treatment 1",]$log_testosterone, 
         y = testosterone[testosterone$treatment == "Treatment 2",]$log_testosterone, 
         ratio = 1)
# F test to compare two variances
# 
# data:  testosterone[testosterone$treatment == "Treatment 1", ]$log_testosterone and testosterone[testosterone$treatment == "Treatment 2", ]$log_testosterone
# F = 2.225, num df = 9, denom df = 9, p-value = 0.2492
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.5526532 8.9577515
# sample estimates:
#   ratio of variances 
# 2.224979 <- too high!

# Doing the anova
aov.logtest <- aov(formula = log_testosterone ~ treatment, data = testosterone)
summary(aov.logtest)
#             Df Sum Sq Mean Sq F value Pr(>F)  
# treatment    1  3.483   3.483   8.193 0.0104 *
# Residuals   18  7.651   0.425                 
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

par(mfrow = c(2, 2))
plot(aov.logtest)
par(mfrow = c(1, 1))
plot(aov.logtest)


##### Going to the main stuffs


# Remove the outliers
testosterone <- testosterone[-c(5, 9),]

# Do the observations of testosterone comply the normal distribution?
library(car)
par(mfrow = c(1, 2))
qqPlot(x = testosterone$testosterone, 
       distribution = 'norm',
       main = 'Q-Q Plot for testosteron, normal distribution',
       xlab = 'norm quantiles',
       ylab = 'testosterone mg/L')
# Drifting values on the right hand side, check again with the lognormal distribution
qqPlot(x = testosterone$testosterone, 
       distribution = 'lnorm',
       main = 'Q-Q Plot for testosteron, log-normal distribution',
       xlab = 'norm quantiles',
       ylab = 'testosterone mg/L')

# There are 2 options from here: Keep working with the variable testosterone, or switch to
# the log_testosterone. The latter should be used since it works on the normal dist. value

# Running a test to see if the log_testosterone follows the normal distribution
par(mfrow = c(1, 1))
qqPlot(x = testosterone$log_testosterone, 
       distribution = 'norm', 
       main = 'Q-Q Plot for log_testosterone, normal distribution', 
       xlab = 'norm quantiles', 
       ylab = 'ln(testosterone)')

# Anderson-Darling test
library(nortest)
ad.test(x = testosterone$log_testosterone)
# Anderson-Darling normality test
# 
# data:  testosterone$log_testosterone
# A = 0.2699, p-value = 0.6347
ad.test(x = testosterone$testosterone)
# Anderson-Darling normality test
# 
# data:  testosterone$testosterone
# A = 0.7968, p-value = 0.03133


# 2.a Descriptive statistics for log(testosterone) and treatment
library(psych)
describeBy(x = testosterone$log_testosterone, 
           group = testosterone$treatment, 
           skew = FALSE, 
           ranges = FALSE)
# group: Treatment 1
# vars n mean   sd   se
# 1    1 8 7.76 0.46 0.16
# ----------------------------------------------------------------------------- 
#   group: Treatment 2
# vars  n mean   sd   se
# 1    1 10 7.23 0.51 0.16


# 2.b find 95% confidence intervals of means and variances
## treatment 1 mean: Please refer to page 196
## in minitab, use stat -> basic stat -> 1 sample t..
logtest_treatment1 <- testosterone[testosterone$treatment == "Treatment 1",]$log_testosterone
logtest_treatment2 <- testosterone[testosterone$treatment == "Treatment 2",]$log_testosterone

t.test(logtest_treatment1)
# One Sample t-test
# 
# data:  logtest_treatment1
# t = 47.7156, df = 7, p-value = 4.645e-10
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   7.371632 8.140354
# sample estimates:
#   mean of x 
# 7.755993

t.test(logtest_treatment2)
# One Sample t-test
# 
# data:  logtest_treatment2
# t = 44.5054, df = 9, p-value = 7.296e-12
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   6.858678 7.593252
# sample estimates:
#   mean of x 
# 7.225965 

## treatment 1 variance
## For this, in minitab, use stat -> basic statistics -> 1 Variance
## R do not have this advantage comparing to Minitab, so I used the built function aboves
var.interval(data = logtest_treatment1)
# Confidence Interval for variance logtest_treatment1 
# 0.09240066 0.8755648 
# Sample estimates:
#   Variance of logtest_treatment1 
# 0.21137

## treatment 2 variance
var.interval(data = logtest_treatment2)
# Confidence Interval for variance logtest_treatment2 
# 0.1247198 0.8785829 
# Sample estimates:
#   Variance of logtest_treatment2 
# 0.2636129


# Question 4: Check if the 2 variances are equal, we perform the F-test:
var.test(x = logtest_treatment1, y = logtest_treatment2, ratio = 1, alternative = "two.sided")
# F test to compare two variances
# 
# data:  logtest_treatment1 and logtest_treatment2
# F = 0.8018, num df = 7, denom df = 9, p-value = 0.7883
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.1910438 3.8673503
# sample estimates:
#   ratio of variances 
# 0.8018197

# use t-test for examining the difference 2 means, equal variances
# alternative hyp. "less" is used here according to means acquired
t.test(x = logtest_treatment1, 
       y = logtest_treatment2, 
       var.equal = TRUE, 
       paired = FALSE, 
       alternative = "two.sided" )

# Two Sample t-test
# 
# data:  logtest_treatment1 and logtest_treatment2
# t = 2.2773, df = 16, p-value = 0.03685
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.03663058 1.02342534
# sample estimates:
#   mean of x mean of y 
# 7.755993  7.225965 


t.test(x = logtest_treatment1, 
       y = logtest_treatment2, 
       var.equal = TRUE, 
       paired = FALSE, 
       alternative = "greater" )

# Two Sample t-test
# 
# data:  logtest_treatment1 and logtest_treatment2
# t = 2.2773, df = 16, p-value = 0.01843
# alternative hypothesis: true difference in means is greater than 0
# 95 percent confidence interval:
#   0.1236822       Inf
# sample estimates:
#   mean of x mean of y 
# 7.755993  7.225965 


# Question 5 - Power curve
# http://www.statmethods.net/stats/power.html
power.t.test(n = 10, delta = .6, sd = .23, sig.level = .05, alternative = 'greater', )
