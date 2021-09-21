###Binomial Test###
##exact method using stats library
library(stats)
##we need a vector showing number of (success, failure) or number of success and total trial
databin = c(10233, 69777)
##we are to test whether the success rate is p = 0.125 
binom.test(databin, p = 0.125)
binom.test(10233, 10233+69777, 0.125) #two-tailed
binom.test(databin, p = 0.125, alternative = "greater") #right-tailed

#problem example from conover book (1999) : in an effort to reduce the side effects from prostate cancer 
#operation which are usually being experienced by at least half of the patients, 
#suppose there is a new method of operation. from 19 trials, 3 people experienced unpleasant side effects. 
#is it safe to conclude that the new method is effective on reducing the side effects from the operation?
#this problem can be viewed as a binomial test problem which alternative hypothesis is left-tailed 
#and probability of .5
binom.test(3, 19, 0.5, alternative = "less") #it can be concluded that the new method is effective 
#in reducing the operation's side effects
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
quantil(19.6,0.5,0.05)

#suppose we want to test whether a certain type of genotype on a plant has the chance of .75 to occurs
#the data consist of genotype "a" appeared on 243 plants while the "b" genotype appeared on 682 plants.
#test is conducted to test for "b" genotype having the probability of .75 to appear on the species of the plant
binom.test(682, 682+243, .75)
quantil(925, .75, .025)#lower quantile
quantil(925, .75, .975)#upper quantile
#using the above quantile, the critical region is 667.94 =< T =<  719.56
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


###Quantile test###
##from : https://people.stat.sc.edu/hitchcock/Rexamples518section3_2.txt
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




library(nortest)
library(KScorrect)
dev13 = c(67.61,28.77,115.00,216.00,15.00,46.00,800.00,190.00,142.00,900.00,17.00,46.00,461.97,4.50,407.42,690.00)
dev14 = c(35.98,30.24,130.00,216.00,15.00,18.00,800.00,222.00,220.00,1350.00,19.00,53.00,324.57,4.50,375.34,935.00)
dev15 = c(26.23,32.80,120.00,177.00,5.00,29.00,2600.00,256.00,168.00,415.00,19.00,37.00,289.73,4.50,304.91,691.00)
ks.test(dev13,dev14)
ks.test(dev13,pnorm)
