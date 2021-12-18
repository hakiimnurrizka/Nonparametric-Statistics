### Nonparametric regression ###
##Overview of linear regression
#nonparametric modelling can be seen as an extension to that of parametric modelling
library(tidyverse)
library(caret)
library(ggplot2)
theme_set(theme_classic())
library(loon.data)
#We'll use bone mineral density data from loon.data package
data("bone")
str(bone)
head(bone, 20)
#applicating linear regression on the data
bone.try = lm(rspnbmd~age+ sex+ ethnic, data = bone)
summary(bone.try)
plot(bone.try, 2)
plot(bone.try, 3)

bone.ano = aov(rspnbmd~sex, data=bone)
bone.ano = aov(rspnbmd~ethnic*sex, data = bone)
summary(bone.ano)
#plot data
ggplot(bone, aes(age, rspnbmd), xl ) + geom_point() +
  ylab("Relative spinal bone mineral density")+ xlab ("age")
#model the data with simple linear regression
lm.bone = lm(rspnbmd~age, data = bone)
summary(lm.bone)
plot(lm.bone, 3)
#Plot the regression line
ggplot(bone, aes(age, rspnbmd), xl ) + geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 1, raw = TRUE))+
  ylab("Relative spinal bone mineral density")+ xlab ("age")
#the regression line seems to oversimplified the data to the point that almost every
#variations of the data is being disregarded by the model.
#next, diagnostic of the residual
plot(lm.bone$residuals, lm.bone$fitted.values,
     ylim = c(-0.06, 0.12),
     ylab = "Residuals", xlab = "Fitted values")
abline(h=c(0), col = "blue")
#from the result above,it can be concluded that a linear function is not suitable to model
#bone density against age. naturally, we might want to improve the model by adding more
#parameter. thus, next the data is modelled with polynomial regression
#split the data into training and test
set.seed(123)
training.samples = bone$rspnbmd %>%
  createDataPartition(p = 0.8, list = FALSE)
train.data  = bone[training.samples, ]
test.data = bone[-training.samples, ]
#chosen polynomial regression model is up to 4th degree
lm.bone2 = lm(rspnbmd ~ age + I(age^2) + I(age^3) + I(age^4), data = bone)
summary(lm.bone2)
#use the model for prediction
predict.bone = lm.bone2 %>% predict(test.data)
#performance of the model
data.frame(RMSE = RMSE(predict.bone, test.data$rspnbmd),
           R2 = R2(predict.bone, test.data$rspnbmd))
#plot the polynomial regression lines to compare the fit
ggplot(bone, aes(x = age, y = rspnbmd), xl ) + geom_point() +
  stat_smooth(method = lm, formula = y ~ x + I(x^2), col = "red")+
  ylab("Relative spinal bone mineral density")+ xlab ("age")
ggplot(bone, aes(x = age, y = rspnbmd), xl ) + geom_point() +
  stat_smooth(method = lm, formula = y ~ x + I(x^2) + I(x^3), col = "green")+
  ylab("Relative spinal bone mineral density")+ xlab ("age")
ggplot(bone, aes(x = age, y = rspnbmd), xl ) + geom_point() +
  stat_smooth(method = lm, formula = y ~ x + I(x^2) + I(x^3) + I(x^4))+
  ylab("Relative spinal bone mineral density")+ xlab ("age")
#degree 4th looks to be the most fitting line for the data.
#and obviously it looks better than the linear one, also from the graph we might even want to
#accept that the model sufficiently represent tha data. however
plot(lm.bone2$fitted.values, lm.bone2$residuals,
     ylab = "Residuals", xlab = "Fitted values")
abline(h=c(0), col = "blue")
#the residual is still showing "pattern" which indicate a heterogenity of variance
#but it is easily countered by using data transformation.
#After reviewing the linear regression, intuitively, some questions would arise and
#the focus in here is : "how to determine the degree of the polynomial?" or in general
#"how to determine the function to use in the model?".
#the previous example we have only 1 predictor but more often than not, model will be 
#built by using more than 1 predictor. Thus, the procedures will be way more difficult
#and conventional model building will be hindered by either time or computation.
#Nonparametric regression offers one of the solution to this problem, that is,
#by using nonparametric method, one can exactly pinpoint what function should
#be used in the model.#

##Monotone regression
#using strictly monotone line/function torepresent the regression line
library(monreg)
age = as.numeric(bone$age)
rspnbmd = as.numeric(bone$rspnbmd)
mon.bone = monreg(age, rspnbmd,hd = .5, hr=.5)
est.monbone = as.data.frame(cbind(mon.bone$t, mon.bone$estimation))
ggplot(est.monbone, aes(V1, V2), xl ) + geom_point()

##Smoothing
x = 1:10
y = c(2,4,6,8,7,12,14,16,18,20)
lo = loess(y~x)
plot(x,y)
lines(predict(lo), col='red', lwd=2)
#nonparametric method to overcome "variations" in the data
library(readxl)
library(graphics)
#weather condition in Semarang
weather_smg = read_excel("preprocess.weather.xlsx")
ggplot(weather_smg[1:20,1:6], aes(x = day, y = humidity)) + 
  geom_col(size = 1, color = "darkblue", fill = "darkblue") +
  geom_line(size = 1.5, color="red", group = 1) +
  stat_smooth(method = loess, formula = y~x, span = 1)
#fuel economy data from ggplot2 package
ggplot(mpg, aes(displ, hwy)) +
  geom_point() +
  geom_smooth(span = 0.3)

#take a look at bone graph again
ggplot(bone, aes(age, rspnbmd), xl ) + geom_point() +
  ylab("Relative spinal bone mineral density")+ xlab ("age")
#looking at the graph from the age axis, we see that beetween value 10 to 15, bone density
#value high variance, then after that, the variance seems to decrease
#intuitively, we come with an idea : making regression model "locally" by 
#splitting data into intervals based on the predictor
with(bone, {
  plot(age, rspnbmd)
  lines(ksmooth(age, rspnbmd, "normal", bandwidth = 0.3), col = 2, lwd = 3)
  lines(ksmooth(age, rspnbmd, "normal", bandwidth = 1), col = 3, lwd = 3)
  lines(ksmooth(age, rspnbmd, "normal", bandwidth = 10), col = 4, lwd = 3)
  legend("topright", legend=c("0.3", "1", "10"),
         col=c("red", "green", "blue"), cex = 0.9, lty = 1, lwd = 3, title = "Bandwidth")})
#the plot above is a kernel smoothing done on  the previous bone data
#there are 3 regression lines, and each of them are smoothed with their corresponding
#"bandwidth". As per intuitive method, splitting the data would require us to decide
#how to split the data. Smoothing regression define this so called "split" with bandwidth
#from the plot, it can be seen that the bigger the bandwidth, the smoother the line is.
#deciding what bandwidth to use is pretty subjective although there are methods to reduce
#such bias, for instance : cross-validation method.
#below are several methods for bandwidth selection in kernel smoothing regression
library(kedd) #from Guidoum, A. C. (2015) for more details : https://cran.r-project.org/web/packages/kedd/vignettes/kedd.pdf
h.mlcv(bone$age)#max-likelihood cross-validation
with(bone, {
  plot(age, rspnbmd)
  lines(ksmooth(age, rspnbmd, "normal", bandwidth = 0.2715), col = 2, lwd = 3)
  legend("topright", legend=c("0.2715"),
         col=c("red"), cex = 0.9, lty = 1, lwd = 3, title = "Bandwidth")})
h.ucv(bone$age) #unbiased cross-validation
with(bone, {
  plot(age, rspnbmd)
  lines(ksmooth(age, rspnbmd, "normal", bandwidth = 0.997), col = 3, lwd = 3)
  legend("topright", legend=c("0.997"),
         col=c("green"), cex = 0.9, lty = 1, lwd = 3, title = "Bandwidth")})
ksmooth.gcv <- function(x, y){
  nobs <- length(y)
  xs <- sort(x, index.return = TRUE)
  x <- xs$x
  y <- y[xs$ix]
  xdif <- outer(x, x, FUN = "-")
  tune.ksmooth <- function(h){
    xden <- dnorm(xdif / h)
    xden <- xden / rowSums(xden)
    df <- sum(diag(xden))
    fit <- xden %*% y
    mean((fit - y)^2) / (1 - df/nobs)^2
  }
  xrng <- diff(range(x))
  oh <- optimize(tune.ksmooth, interval = c(xrng/nobs, xrng))$minimum
  if(any(oh == c(xrng/nobs, xrng)))
    warning("Minimum found on boundary of search range.\nYou should retune model with expanded range.")
  xden <- dnorm(xdif / oh)
  xden <- xden / rowSums(xden)
  df <- sum(diag(xden))
  fit <- xden %*% y
  list(x = x, y = fit, df = df, h = oh)
}
bone.kern = with(bone, ksmooth.gcv(age, rspnbmd)) 
bone.kern$h#general cross-validation
with(bone, {
  plot(age, rspnbmd)
  lines(ksmooth(age, rspnbmd, "normal", bandwidth = 0.9212), col = 4, lwd = 3)
  legend("topright", legend=c("0.9212"),
         col=c("blue"), cex = 0.9, lty = 1, lwd = 3, title = "Bandwidth")})
#thus, it can be deducted that optimal bandwidth value for gaussian-kernel smoothing regression
#on bone mineral density data is around 0.9-1
with(bone, {
  plot(age, rspnbmd, ylab = "Relative spinal bone mineral density")
  lines(bone.kern, lwd = 3, col = "blue")
  legend("topright", legend=c("0.9212"),cex = 0.9, title = "Bandwidth")})
plot(bone.kern$x, bone.kern$y)

#next, we'll apply spline smoothing. spline smoothing use the spline interpolation method
#to estimate the functional relationship between predictor and goal.
#similar to kernel smoothing, spline formula requires a coefficient to specify
#closeness on making the neighbors. In spline its referred as span
#here is the default for spline smoothing (ss) algorithm in npreg package. it uses cubic
#spline
bone.spline = with(bone, ss(age, rspnbmd))
bone.spline
summary(bone.spline)
with(bone, {
  plot(age, rspnbmd, ylab = "Relative spinal bone mineral density")
  lines(bone.spline$x,bone.spline$y, lwd = 3, col = "red")
})
#changing some of the parameters on the function 
bone.spline2 = with(bone, ss(age, rspnbmd, nknots = 8, m = 2, spar = 0.16))
summary(bone.spline2)
with(bone, {
  plot(age, rspnbmd, ylab = "Relative spinal bone mineral density")
  lines(bone.spline$x,bone.spline$y, lwd = 3, col = "red")
  lines(bone.spline2$x,bone.spline2$y, lwd = 3, col = "green")
})
#smoothing regression line with 95% bayesian confidence interval
plot(bone.spline, xlab = "age", ylab = "Relative spinal bone mineral density")
#compare with kernel result
plot(age, rspnbmd)
ix = sort(age, index.return=T)$ix

with(bone, {
  plot(age, rspnbmd, ylab = "Relative spinal bone mineral density")
  lines(age[ix], predict(lm.bone2)[ix], lwd = 3, col = "green")
  lines(bone.kern, lwd = 3, col = "blue")
  lines(bone.spline$x,bone.spline$y, lwd = 3, col = "red")
  legend("topright", legend=c("Polynomial", "Kernel smoothing", "Spline smoothing")
         ,cex = 0.9, lwd = c(3,3,3) , col = c("green", "blue","red"), title = "Type of line")})

##On a monotonic data##
first_cases = read_excel("~/Nonparametric-Statistics/first-cases.xlsx")
library(ggplot2)
library(monreg)
library(corrplot)
fc_cor = cor(first_cases)
corrplot(fc_cor, method = "number")
ggplot(first_cases, aes(day, new)) + geom_point() 
ggplot(first_cases, aes(day, total)) + geom_point() 
#linear regression
fc_lm = lm(new~day, data = first_cases)
summary(fc_lm)
plot(fc_lm, 1)
#kernel
with(first_cases, {
  plot(day, new)
  lines(ksmooth(day, new, "normal", bandwidth = 5), col = 2, lwd = 3)
  lines(ksmooth(day, new, "normal", bandwidth = 10), col = 3, lwd = 3)
  lines(ksmooth(day, new, "normal", bandwidth = 15), col = 4, lwd = 3)
  legend("topright", legend=c("5", "10", "15"),
         col=c("red", "green", "blue"), cex = 0.6, lty = 1, lwd = 3, title = "Bandwidth")})

attach(first_cases)
fc_mon = monreg(day, total, hd = .5, hr=.5)
fc_mon.est = as.data.frame(cbind(fc_mon$t, fc_mon$estimation))
ggplot(fc_mon.est, aes(V1, V2)) + geom_line() + xlab("day")+ ylab("estimation")