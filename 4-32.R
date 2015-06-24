library(BSDA)

# 4.32
# H0: mu = 100 vs H1: mu > 100
# stdev = 2.4
# N = 25
# Mean = 101.560
z.test(x = rep(101.560, 25), 
       alternative = "greater", 
       mu = 100, 
       sigma.x = 2.4, 
       conf.level = .95)
#   One-sample z-Test
# 
# data:  rep(101.56, 25)
# z = 3.25, p-value = 0.000577
# alternative hypothesis: true mean is greater than 100
# 95 percent confidence interval:
#   100.7705       NA
# sample estimates:
# mean of x 
#    101.56 

# a.
# SE Mean = stdev / sqrt(N) = 2.4 / sqrt(25) = .48
# .95 CI lower bound = 100.770
# Z = 3.25 
# P = ?
pnorm(3.25, lower.tail = FALSE) # = ..000577025 < alpha = .05
# The null hypothesis cannot be rejected at .05 level

# b. This is a one sided test since H1: mu > 100

# c. H0 mu = 99 vs H1: mu > 99
# We can see that mu = 99 < .95 Lower Bound CI = 100.770 -> there is a high chance of rejection
# of the null hypothesis. z-test agree with this answer by the p-value < .05:
z.test(x = rep(101.560, 25), 
       alternative = "greater", 
       mu = 99, 
       sigma.x = 2.4, 
       conf.level = .95)
# 
#   One-sample z-Test
# 
# data:  rep(101.56, 25)
# z = 5.3333, p-value = 4.821e-08
# alternative hypothesis: true mean is greater than 99
# 95 percent confidence interval:
#   100.7705       NA
# sample estimates:
# mean of x 
#    101.56 


# d. Use the output and the normal table to find a 95% two-sided CI on the mean
mean.interval = function(data= NA , 
                         mean = mean(data), 
                         stdev = sd(data), 
                         n = length(data), 
                         conf.level = .95){
  z.CI2 = qnorm((1 - conf.level)/2)
  z.CI1 = qnorm(1 - conf.level)
  
  CI2l = mean - z.CI2 * stdev/sqrt(n)
  CI2u = mean + z.CI2 * stdev/sqrt(n)
  CI_U = mean + z.CI1 * stdev/sqrt(n)
  CI_L = mean - z.CI1 * stdev/sqrt(n)
  
  cat('\t Confiedence Interval (CI) for normal-distribution mean ',
      mean, ': \n',
      'two-sided CI: \n',
      CI2u, '\t',
      CI2l, '\n',
      '\ 1 sided - Lower Bound CI: \n',
      CI_L, '\n',
      '\ 1 sided - Upper Bound CI: \n',
      '\t\t', CI_U)
}
mean.interval(mean = 101.560, stdev = 2.4, n = 25)
# Confiedence Interval (CI) for normal-distribution mean  101.56 : 
#   two-sided CI: 
#   100.6192    102.5008 
# 1 sided - Lower Bound CI: 
#   102.3495 
# 1 sided - Upper Bound CI: 
#   100.7705

z.test(x = rep(101.560, 25), 
       alternative = "greater", 
       mu = 99, 
       sigma.x = 2.4, 
       conf.level = .95)
# One-sample z-Test
# 
# data:  rep(101.56, 25)
# z = 5.3333, p-value = 4.821e-08
# alternative hypothesis: true mean is greater than 99
# 95 percent confidence interval:
#   100.7705       NA
# sample estimates:
#   mean of x 
# 101.56 
z.test(x = rep(101.560, 25), 
       alternative = "less", 
       mu = 99, 
       sigma.x = 2.4, 
       conf.level = .95)
# One-sample z-Test
# 
# data:  rep(101.56, 25)
# z = 5.3333, p-value = 1
# alternative hypothesis: true mean is less than 99
# 95 percent confidence interval:
#   NA 102.3495
# sample estimates:
#   mean of x 
# 101.56 
z.test(x = rep(101.560, 25), 
       alternative = "two.sided", 
       mu = 99, 
       sigma.x = 2.4, 
       conf.level = .95)

# One-sample z-Test
# 
# data:  rep(101.56, 25)
# z = 5.3333, p-value = 9.643e-08
# alternative hypothesis: true mean is not equal to 99
# 95 percent confidence interval:
#   100.6192 102.5008
# sample estimates:
#   mean of x 
# 101.56