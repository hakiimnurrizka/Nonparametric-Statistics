### Kolmogorov-Smirnov type of tests ###
##KS goodness of fit test for one sample
#this test use empirical distribution of the sample data and compare it to the hypothesized
#distribution
#The test statistics is built from the difference of the Empirical distribution(s)
#See : Conover, W. J. (1999) p. 430-431
#for this example, the data is tested for the hypothesis that it comes from normal distribution
library(nortest)
library(KScorrect)
dev13 = c(67.61,28.77,115.00,216.00,15.00,46.00,800.00,190.00,142.00,900.00,17.00,46.00,461.97,4.50,407.42,690.00)
dev14 = c(35.98,30.24,130.00,216.00,15.00,18.00,800.00,222.00,220.00,1350.00,19.00,53.00,324.57,4.50,375.34,935.00)
dev15 = c(26.23,32.80,120.00,177.00,5.00,29.00,2600.00,256.00,168.00,415.00,19.00,37.00,289.73,4.50,304.91,691.00)

ks.test(dev13,pnorm)
ks.test(dev14, pnorm, alternative = "less")
ks.test(dev15, pnorm, alternative = "greater")

##KS goodness of fit for two sample (also known as Smirnov test)
#a type of KS to test whether two samples come from the same distribution
ks.test(dev13, dev14)
ks.test(dev13, dev15)
ks.test(dev14, dev15)
treat.a = c(-14, -62, -38, -19, -21, -28, -32, -40)
treat.b = c(-51, -31, 14, -12, -27, -38, -10, 6)
ks.test(treat.a, treat.b)

##Cramer von Mises test
#Similarly to KS test, CVM test is also used for both one sample and two sample
#Biggest difference is CVM use a standardized sum of difference on the empirical distribution(s)
#See : Conover, W. J. (1999) p. 463
library(goftest)
library(twosamples)
program = as.factor(c("a","a","b","b","b","a","b","a","a","a",
                      "b","b","b","a","a","a","a","b","b","b"))
saving = c(143, 106, 182, 158, 161, 108, 131, 138, 101, 83, 
           175, 142, 111, 82, 12, 58, 42, 96, 90, 144)
saving.program = as.data.frame(cbind(program, saving))
saving.program[order(saving.program$program),]
cvm.test(saving.program, null = "pexp")

sav.prog.a = saving.program[which(saving.program$program==1),2]
sav.prog.b = saving.program[which(saving.program$program==2),2]

cvm_test(sav.prog.a, sav.prog.b)


###Goodness of fit test for family of distribution
##Lilliefors
#testting goodness of fit to certain family distribution
#test statistic is derived from KS goodness of fit, the only adjustment is that each
#point of the sample is transformed and the transformed data is used instead of the actual data
#Ex. : for lilliefors normality test, the data X is tranformed using the standardization Z = (X-Xbar)/s
#with Xbar is mean of the data X dan s is the standar deviation of X.
#for more details see : Conover, W. J. (1999), p. 443-444
#following exaples is using normal distribution as the default function
lillie.test(dev13)
lillie.test(treat.b)
lillie.test(sav.prog.a)
#other family distribution is also applicable
library(KScorrect)
LcKS(dev13, "pexp", nreps = 1000) #exponential
LcKS(treat.b, "punif", nreps = 1000) #uniform


##Shapiro Wilk
#goodness of fit normal distribution test
#test statistics is built from weighted difference of the ordered data(see Conover, 1999; p.450)
shapiro.test(dev13)
shapiro.test(treat.b)
shapiro.test(sav.prog.a)
