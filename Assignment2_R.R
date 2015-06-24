library(XLConnect)
library(nortest)

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

# This function is for use for workout the mean and variance of a lognormal distribution typed
# data from its normal distribution perspective.
mean.lnorm = function(data, mean = mean(data), sd = sd(data)) {
  result <- exp(mean + (sd^2)/2)
  return(result)
}

# We need to change the treatment to factor!
# testosterone$treatment <- factor(testosterone$treatment)


# Create a new column named log_testosterone = log(testosterone) of exp(1)
testosterone$log_testosterone <- log(x = testosterone$testosterone, base = exp(1))
# testosterone <- testosterone[-c(5, 9),]

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
# A = 0.4778, p-value = 0.2106
ad.test(x = testosterone[testosterone$treatment == "Treatment 1", "log_testosterone"])
ad.test(x = testosterone[testosterone$treatment == "Treatment 2", "log_testosterone"])
ad.test(x = testosterone$testosterone)
# histogram for log_testosterone with fit
# 2 histograms in 1 graph?
hist(x = testosterone$log_testosterone, 
     freq = FALSE, 
     breaks = 12, 
     xlab = 'ln(testosterone)', 
     main = 'Histogram of ln(testosterone)')
lines(x = density(testosterone$log_testosterone, bw = sd(testosterone$log_testosterone)))


# 2.a Descriptive statistics for log(testosterone) and treatment
library(pastecs)
logtest_treatment1 <- testosterone[testosterone$treatment == "Treatment 1",]$log_testosterone
logtest_treatment2 <- testosterone[testosterone$treatment == "Treatment 2",]$log_testosterone
stat.desc(x = cbind(logtest_treatment1, logtest_treatment2), 
          basic = TRUE, 
          p = .95)
#              logtest_treatment1 logtest_treatment2
# median               7.91782422         7.12004110
# mean                 8.06056848         7.22596504
# SE.mean              0.24218445         0.16236160
# CI.mean.0.95         0.54785928         0.36728745
# var                  0.58653306         0.26361288
# std.dev              0.76585447         0.51343245
# coef.var             0.09501246         0.07105382

# library(psych)
# describe(x = cbind(logtest_treatment1, logtest_treatment2))



# Test for the difference between the means of 2 groups. 
t.test(x = logtest_treatment1, 
       y = logtest_treatment2, 
       alternative = "greater", 
       var.equal = FALSE, 
       conf.level = .95)
# Welch Two Sample t-test
# 
# data:  logtest_treatment1 and logtest_treatment2
# t = 2.8624, df = 15.73, p-value = 0.9943
# alternative hypothesis: true difference in means is less than 0
# 95 percent confidence interval:
#   -Inf 1.344191
# sample estimates:
#   mean of x mean of y 
# 8.060568  7.225965 


# Test for the differences between the varianecs of the 2 groups
var.test(x = logtest_treatment1, 
         y = logtest_treatment2, 
         alternative = "two.sided", 
         conf.level = .95)



# 2.b find 95% confidence intervals of means and variances
## treatment 1 mean: Please refer to page 196
## in minitab, use stat -> basic stat -> 1 sample t..
t.test(logtest_treatment1)

# One Sample t-test
# 
# data:  logtest_treatment1
# t = 33.2828, df = 9, p-value = 9.829e-11
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   7.512709 8.608428
# sample estimates:
# mean of x 
# 8.060568 

## treatment 2 mean (same with treatment 1)
t.test(logtest_treatment2) 
# One Sample t-test
# 
# data:  logtest_treatment2
# t = 44.5054, df = 9, p-value = 7.296e-12
# alternative hypothesis: true mean is not equal to 0
# 95 percent confidence interval:
#   6.858678 7.593252
# sample estimates:
# mean of x 
# 7.225965 

## treatment 1 variance
## For this, in minitab, use stat -> basic statistics -> 1 Variance
## R do not have this advantage comparing to Minitab, so I used the built function aboves
# var.interval(data = logtest_treatment1)
# Confidence Interval for variance logtest_treatment1 
# 0.2774989 1.954828 
# Sample estimates:
# Variance of logtest_treatment1 
# 0.5865331

## treatment 2 variance
var.interval(data = logtest_treatment2)
# Confidence Interval for variance logtest_treatment2 
# 0.1247198 0.8785829 
# Sample estimates:
# Variance of logtest_treatment2 
# 0.2636129

# 2.c convert means and confidence intervals back to testosterone
# refers back to page 134. 
mean(testosterone$testosterone)
exp(x = mean(testosterone$log_testosterone))

# 3. Test for the differences between 2 means/variances' groups.
# The strategy for this question is basically, we have to do the poold t-test for the experimental
# data; however, it is not certain that the variances between 2 groups are the same. What we need
# to do is to perform a test to see if the variances are equal (F-test). If it is, then we perform
# the pooled t-test with equal variance condition; otherwise, the pooled t-test should be done
# without the same variance as input.

# Firstly, creating a dot plot to see the pattern of the data:
# Dot plot for 2 treatments for having an overview of the data characteristics
# par(mfrow = (c(1, 2)))
# library(ggplot2)
# ggplot(data = testosterone, 
#        aes(x = treatment, 
#            y = log_testosterone, 
#            fill = treatment)) + 
#   geom_boxplot() + 
#   ylab('ln(testosterone)')
stripchart(x = log_testosterone ~ treatment, 
           data = testosterone, 
           method = 'jitter', 
           offset = .5, 
           pch = 21, 
           vertical = TRUE)

# The plot shows that there are different in the distribution positions of the 2 treatment groups,
# which means that the means between them are possibly different. Moreover, the spreads of data in
# the plotting space are also different (with treatment 1, the observations are more varied than
# those belong to treatment 2)


# Check if the 2 variances are equal, we perform the F-test:
var.test(x = logtest_treatment2, y = logtest_treatment1, ratio = 1, alternative = "two.sided")
# F test to compare two variances
# 
# data:  logtest_treatment1 and logtest_treatment2
# F = 2.225, num df = 9, denom df = 9, p-value = 0.2492
# alternative hypothesis: true ratio of variances is not equal to 1
# 95 percent confidence interval:
#   0.5526532 8.9577515
# sample estimates:
# ratio of variances 
# 2.224979

# library(lawstat)
# levene.test(y = testosterone$log_testosterone, group = testosterone$treatment, location = 'median')
library(car)
leveneTest(testosterone~treatment, center = 'median', data = testosterone)
# Levene's Test for Homogeneity of Variance (center = "median")
#       Df F value  Pr(>F)  
# group  1  3.0515 0.09771 .
#       18                  
# ---
# Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# With the p-value = .2492 > significance level = .05 (nearly 5 times), we have a conclusion that
# the null hypothesis (ratio of sigma = 1) should not be rejected and should be considered during
# the rest of the test

# use t-test for examining the difference 2 means, in this case, we should run the 2 t-tests:
# with equal variances and unequal variances.
t.test(x = logtest_treatment1, 
       y = logtest_treatment2, 
       var.equal = FALSE, 
       paired = FALSE, 
       alternative = "two.sided" )
# data:  logtest_treatment1 and logtest_treatment2
# t = 2.8624, df = 15.73, p-value = 0.01143
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.2156351 1.4535718
# sample estimates:
# mean of x mean of y 
#  8.060568  7.225965 

t.test(x = logtest_treatment1, 
       y = logtest_treatment2, 
       var.equal = TRUE, 
       paired = FALSE, 
       alternative = "two.sided" )

# Two Sample t-test
# 
# data:  logtest_treatment1 and logtest_treatment2
# t = 2.8624, df = 18, p-value = 0.01035
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   0.2220321 1.4471748
# sample estimates:
# mean of x mean of y 
#  8.060568  7.225965

# power.t.test(n = 10, delta = .85, sd = .02 )
# library(pwr)
# pwr.t.test(n = 10)

power.t.test(n = c(10, 12, 15, 20), delta = .85, sd = .02, sig.level = .05, type = "two.sample", alternative = "one.sided")

# Two-sample t test power calculation 
# 
#               n = 10, 12, 15, 20
#           delta = 0.85
#              sd = 0.02
#       sig.level = 0.05
#           power = 1, 1, 1, 1
#     alternative = one.sided
# 
# NOTE: n is number in *each* group


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
par(mfrow(1, 1))
plot(aov.logtest)
