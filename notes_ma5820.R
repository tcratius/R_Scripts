#************
# Review probability of a population is inclusive, mutatlly exlusive p 212
#***********

#************
# Making simulations of population mean
#***********
mu <- 100; sigma <- 16
x <- rnorm(16, mu, sigma) 
M <- 4; n <- 16 # M for more

#************
# (1) Randomly generates sample distribution around the mean
# based on sample size (n), population mu and sigma
#***********
res <- numeric(M)
for (i in 1:M) {
  res[i] <- mean(rnorm(n, mean=mu, sd=sigma))
} 
res
# same thing as above (1)
xbar <- function(i) mean(rnorm(n, mean=mu, sd=sigma))
sapply(1:M, xbar)

# same as (1)
Xbar <- Vectorize(xbar)
Xbar(1:M)

# (1)
replicate(M, mean(rnorm(n, mean=mu, sd=sigma)))

# creates columns and rows of a matrix first
# 16 rows 4 columns
x <- matrix(rnorm(M*n, mean=mu, sd=sigma), nrow=n)
dim(x)

# 2 indicated the margin to be rows, 1 for columns
apply(x, MARGIN = 2, mean) 

#************
# For approximation a normal population, the sampling distribution of
# x bar is also normal distributed. We will verify this through a simulation.
#***********

#************
# function to find z-score of sample distribution of xbar. This shows how much the
# sample mean deviates +/- compared with the population mu.  As sampling increases
# xbar should get closer to mu. If you are lucky enought to know the population mu
# and sigma then they can be used to get a greater understanding of how far the 
# sample mean deviates for the true population.
#***********
zstat <- function(x, mu, sigma) {
  (mean(x)-mu)/(sigma/sqrt(length(x))) # remember under unknown pop mu = 0 and sd =1
  
}

M <-2000; n <- 7
mu <- 100; sigma <- 16
res <- replicate(M, {
  x=rnorm(n, mean=mu, sd=sigma)
  zstat(x, mu, sigma)
})

qqnorm(res, main="Normal, n=7")

#************
# A natural question is: what n is large enough? The answer depends on the
# population— populations which are either skewed or long-tailed will need
# larger values of n . Let’s look at the right-skewed exponential distribution.
# We need to know that it has population mean and population standard deviation
# are both given by 1/rate: (Verzani, p.247, 2014)
#***********

M <- 2000; n <- 7 # change to 150 to normalise.
rate <- 2; mu <- sigma = 1/rate
res <- replicate(M, {
  x=rexp(n, rate=rate) # rexp for exponential distribution
  zstat(x, mu, sigma)
})
qqnorm(res, main="Exponential, n=7") 
# This is indicative of a right-tailed distribution.
# Increase the rate of n i.e. n <- 150 and the sample distrubtion will normalise.

#************
# If the population is unknown than the tstat can be used to work out how far the
# sample mean distribution deviates from the predicted population.  In this scenario
# Population = 1, mu = 0, sigma = 1.
#***********
tstat <- function(x, mu) {
  (mean(x) - mu)/(sd(x)/sqrt(length(x)))
}

#*************
# As s is random, the distribution of the t -statistic should differ from that 
# of the z -statistic: we would expect it to have longer tails as larger values 
# are produced when sample standard deviation < mu but also that this difference 
# decreases for larger values of n.
# ***********
mu <- 0; sigma <- 1
M <- 750; n <- 4
res <- replicate(M, tstat(rnorm(n, mu, sigma), mu))
boxplot(res)

#************
# Mean vs Median of a replicated normal distribution sample size 35 of 
# 1000 samples of the sample mean.  Both are centred around 0 however the median
# shows more variability than the mean. 
#***********
M <- 1000; n <- 35
res_mean <- replicate(M, mean(rnorm(n)))
res_median <- replicate(M, median(rnorm(n)))
boxplot(list("Sample Mean"=res_mean, "Sample Median"=res_median),
        main="Normal Population")

# Below is an example that shows the sample mean and median having comparable 
# spreads? when the population has an exponential distribution.
M <- 1000; n <- 35
res_mean <- replicate(M, mean(rexp(n))) -1
res_median <- replicate(M, median(rexp(n))) - log(2)
boxplot(list("Sample mean"=res_mean, "Sample median"=res_median),
        main="Exponential Population")

#************
# p 252 confirming the result of test using confidence intervals
#***********

mu <- 28.6 ; sigma <- 6.3 
M <- 1000 ; n <- 4 
res <- replicate(M, { 
  x <- rnorm(n, mu, sigma) 
  SE <- sd(x)/sqrt(n) # standard error
  (mean(x) - mu) / SE 
}) 

quantile(res, c(0.025, 0.975))
#*************
#   2.5%      97.5% 
#  -2.819887  3.340365
# xbar 28.6 - 2.8 . SE < u < xbar 28.6 + 3.3. SE 
# From capstone ma5820
# This is an example, however it assumes we know the population u, mu, sigma.
#************

#************
# This section briefly touches on a computer-intensive means to work around the
# absence of a known sampling distribution, the bootstrap method 
# Verzani, p.258, 2014,
#***********

#************
# Bayes theorum, need a better example, book does not make much sense.
#***********


