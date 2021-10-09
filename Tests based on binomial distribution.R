###Tests based on Binomial distribution###
##exact binomial test method using stats library
library(stats)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(datarium)
#we need a vector showing number of (success, failure) or number of success and total trial
databin = c(10233, 69777)
#we are to test whether the success rate is p* = 0.125 
binom.test(databin, p = 0.125)
binom.test(10233, 10233+69777, 0.125) #two-tailed
binom.test(databin, p = 0.125, alternative = "greater") #right-tailed

#problem example from conover book (1999) : in an effort to reduce the side effects from prostate cancer 
#operation which are usually being experienced by at least half of the patients, 
#suppose there is a new method of operation. from 19 trials, 3 people experienced unpleasant side effects. 
#is it safe to conclude that the new method is effective on reducing the side effects from the operation?
#this problem can be viewed as a binomial test problem which alternative hypothesis is left-tailed 
#and probability of .5
binom.test(16, 19, 0.5, alternative = "greater") #it can be concluded that the new method is effective 
#in reducing the operation's side effects ; H1 : side effects experienced by < .5 from total patients
binom.test

##using approximation with normal distribution
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
quantil(19,0.5,0.05)

#suppose we want to test whether a certain type of genotype on a plant has the chance of .75 to occurs
#the data consist of genotype "a" appeared on 243 plants while the "b" genotype appeared on 682 plants.
#test is conducted to test for "b" genotype having the probability of .75 to appear on the species of the plant
binom.test(682, 682+243, .75)
quantil(925, .75, .025)#lower quantile
quantil(925, .75, .975)#upper quantile
#using the above quantile, the critical region is 667.94 > T and T >  719.56
#pvalue from normal distribution approximation can be derived as
pnorm((682-.75*925+.5)/sqrt(925*.75*.25))

#confidence interval for probability or proportion of population
conf_int = function(y,n,alpha){
  DNAME <- deparse1(substitute(y))
  yr <- round(y)
  if (any(is.na(y) | (y < 0)) || max(abs(y - yr)) > 1e-07) 
    stop("'y' must be nonnegative and integer")
  y <- yr
  if (length(y) == 2L) {
    n <- sum(y)
    y <- y[1L]
  }
  else if (length(y) == 1L) {
    nr <- round(n)
    if ((length(n) > 1L) || is.na(n) || (n < 1) || abs(n - 
                                                       nr) > 1e-07 || (y > nr)) 
      stop("'n' must be a positive integer >= 'y'")
    DNAME <- paste(DNAME, "and", deparse1(substitute(n)))
    n <- nr
  }
  else stop("incorrect length of 'y'")
  if (!missing(alpha) && (length(alpha) > 1L || is.na(alpha) || alpha < 0 || 
                      alpha > 1)) 
    stop("'alpha' must be a single number between 0 and 1")
  quant_diff = qnorm(1-(alpha/2))*sqrt(y*(n-y)/n^3)
  upper = y/n+quant_diff
  lower = y/n-quant_diff
  structure(list(Upper = upper, Lower = lower))
}

conf_int(4, 20, .05)


##Quantile test
#from : https://people.stat.sc.edu/hitchcock/Rexamples518section3_2.txt#
quantile.test<-function(x,xstar=0,quantile=.5,alternative="two.sided"){
  n<-length(x)
  p<-quantile
  T1<-sum(x<=xstar)
  T2<-sum(x< xstar)
  if (alternative=="quantile.less") {
    p.value<-1-pbinom(T2-1,n,p)}
  if (alternative=="quantile.greater"){
    p.value<-pbinom(T1,n,p)}
  if (alternative=="two.sided"){
    p.value<-2*min(1-pbinom(T2-1,n,p),pbinom(T1,n,p))}
  list(xstar=xstar,alternative=alternative,T1=T1,T2=T2,p.value=p.value)
  }

#an example case for this test is, say there are students with their test score recorded. 
#then we would like to inspect whether the upper quartile of the scores is 193
testscores <- c(189,233,195,160,212,176,231,185,199,213,202,193,174,166,248)

quantile.test(testscores,xstar=193,quantile=0.75,alternative="two.sided")
#thus it can be concluded that the upper quartile of the scores is not 193 

#another example is for testing one-tailed quantile.
#suppose we have prices for houses within a neighborhood, then we want to test whether the median
#of the house price is at least 179 
prices <- c(120, 500, 64, 104, 172, 275, 336, 55, 535, 251, 214, 1250, 402, 27, 109, 17, 334, 205)

quantile.test(prices,xstar=179, quantile=0.5, alternative="quantile.less")
sort(prices)
##confidence interval for quantile
#to make the (1-alpha) confidence interval for certain quantile
quantile.interval<-function(x,quantile=.5,conf.level=.95){
  n<-length(x)
  p<-quantile
  alpha<-1-conf.level
  rmin1<-qbinom(alpha/2,n,p)-1
  r<-rmin1+1
  alpha1<-pbinom(r-1,n,p)
  smin1<-qbinom(1-alpha/2,n,p)
  s<-smin1+1
  alpha2<-1-pbinom(s-1,n,p)
  clo<-sort(x)[r]
  chi<-sort(x)[s]
  conf.level<-1-alpha1-alpha2
  list(quantile=quantile,conf.level=conf.level,r=r,s=s,interval=c(clo,chi))}
#example with the house prices : 95% CI for median
quantile.interval(prices, .5, .95)
#example with the house prices : 90% CI for .7 quantile
quantile.interval(prices, .7, .9)

###cox-stuart
#testing randomness/trend of data
library(randtests)
set.seed(100)
crab_farm = c(rbinom(20,2000, 0.7))#suppose that it is data of crab harvest within 20 months of observation on a certain crab farm
cox.stuart.test(crab_farm)#turns out there is no significant difference in the crab harvest within 20 months

##sign test
#a test to see whether one variable tend to have higher values than the other variable.
#in a sense sign test is simply another kind of binomial test(p*=.5). the importance to be remembered is
#this test, most of the time, is used for paired data. thus, it is also similar to t-test for paired samples
data(mice2)
mice2 #from datarium library
#transform data by gathering the value of "before" and "after" in a column
mice2 = mice2 %>%
  gather(key = "group", value = "weight", before, after) 
#summarize data
mice2 %>% group_by(group) %>% summarise(n = length(weight),  
                                        mean_weight = mean(weight),
                                        median_weight = median(weight),
                                        iqr = IQR(weight))
#based on the above summary, it can be seen the big difference in mean and median values for the 
#weight of specimens before and after treatment
#now we use the statistics test for sign test
mice2
sign_test(mice2, weight ~ group, alternative = "greater")
#based on the result above, it can be concluded that the values of weight from the specimens whom
#received treatment tend to be higher than the weight of the specimens whom has not received the treatment
binom.test(8,9, alternative = "greater")
binom.test(82,82)
binom.test(9,10, alternative = "greater")
##sign test exercise from chapter 3.4 W. J. Conover - Practical Nonparametric Statistics, 3rd (1999, Wiley)
# 2
binom.test(22,26, alternative = "greater") #statistics test shows a significant evidence of
#reaction time after lunch being longer than reaction time before lunch
# 3
binom.test(77,100) #evidently there is significant difference of durability affected by two of the additives
# 4
binom.test(7, 19) #there is not enough evidence of significant difference in preferences

##mcnemar test(test of change significance)
#variation from sign test in which instead of ordinal, we have categorical variables
#another way to see it is suppose we have bivariate variable (joint variables) independent with each other
#then for each of these individual variables has exactly 2 categories (lets call it 0 and 1)
#illustration : we have two surveys conducted asking whether choosing between 2 candidates of the election. 
#the first survey conducted just before the candidates debate took place and the second one was held after the debate.
vote = matrix(c(144, 29, 31, 431), 2,2, byrow = T)
vote
#after obtaining the data, we would like to conduct a test whether the debate has significant effect to
#people's vote or not : 
mcnemar.test(vote)
binom.test(29,60)