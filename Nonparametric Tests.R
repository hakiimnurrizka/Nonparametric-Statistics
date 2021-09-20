###Binomial Test###
##using stats library
library(stats)
##we need a vector showing number of (success, failure) or number of success and total trial
databin = c(10233, 69777)
##we are to test whether the success rate is p = 0.125 
binom.test(databin, p = 0.125)
binom.test(10233, 10233+69777, 0.125) #two-tailed
binom.test(databin, p = 0.125, alternative = "greater") #right-tailed
##problem example from conover book (1999) : in an effort to reduce the side effects from prostate cancer 
##operation which are usually being experienced by at least half of the patients, 
##suppose there is a new method of operation. from 19 trials, 3 people experienced unpleasant side effects. 
##is it safe to conclude that the new method is effective on reducing the side effects from the operation?
#this problem can be viewed as a binomial test problem which alternative hypothesis is left-tailed 
#and probability of .5
binom.test(3, 19, 0.5, alternative = "less") #it can be concluded that the new method is effective 
#in reducing the operation's side effects
binom.test

#test statistic using quantile based on normal distribution
quantil = function(n,p,q){
  nr <- round(n)
  if (any(is.na(n) | (n < 0)) || max(abs(n - nr)) > 1e-07) 
    stop("'n' must be nonnegative and integer")
  n <- nr
  if (!missing(p) && (length(p) > 1L || is.na(p) || p < 0 || 
                      p > 1)) 
    stop("'p' must be a single number between 0 and 1")
  if (!missing(q) && (length(q) > 1L || is.na(q) || q < 0 || 
                      q > 1)) 
    stop("'q' must be a single number between 0 and 1")
  quant = n * p + qnorm(q)*sqrt(n*p*(1-p))
  structure(quant)
}

quantil(19.6,0.5,0.05)



library(nortest)
library(KScorrect)
dev13 = c(67.61,28.77,115.00,216.00,15.00,46.00,800.00,190.00,142.00,900.00,17.00,46.00,461.97,4.50,407.42,690.00)
dev14 = c(35.98,30.24,130.00,216.00,15.00,18.00,800.00,222.00,220.00,1350.00,19.00,53.00,324.57,4.50,375.34,935.00)
dev15 = c(26.23,32.80,120.00,177.00,5.00,29.00,2600.00,256.00,168.00,415.00,19.00,37.00,289.73,4.50,304.91,691.00)
ks.test(dev13,dev14)
ks.test(dev13,pnorm)
