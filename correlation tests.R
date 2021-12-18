### Nonparametric Correlation test ###
##Spearman
#based on pearson correlation formula but uses the rank instead of the actual value
#see : Conover W. J. (1999), p. 314-317
husband = c(147, 158, 131, 142, 183, 151, 196, 129, 155, 158)
wife = c(122, 128, 125, 123, 115, 120, 108, 143, 124, 123)
stat.x = c(-8.7, -8.3, -8.2, -7.2, -6.1, -6.0, -4.1, -2.0, -1.9, -1.6, -1.3, -0.2,
           0.7, 1.3, 1.6, 2.1, 2.2, 4.0, 5.6, 5.9, 6.2, 6.6, 6.7, 8.1)
stat.y = c(-0.6, -0.8, -1.3, -1.9, -2.0, -2.1, -4.0, -4.6, -4.7, -5.5, -5.6, -6.0, 
           4.6, 4.4, 4.2, 3.9, 3.8, 3.5, 3.1, 2.6, 2.0, 1.2, 0.6, 0.4)

cor.test(husband, wife, method = "spearman")
cor.test(stat.x, stat.y, method = "spearman", alternative = "greater")

##Kendall
#formula is based on the concordance pairs
#see : Conover W. J. (1999), p. 319-321
cor.test(husband, wife, method = "kendall")
cor.test(stat.x, stat.y, method = "kendall", alternative = "greater")

##Daniel test for trend(spearman on time series data)
#Proposed by using spearman correlation formula to check trend of value in a time series data
#Alternative hypothesis is that the value will change overtime (either increase or decreaseas the time goes by)
library(trend)
defect = c(6.1, 7.5, 7.7, 5.9, 5.2, 6.1, 5.3, 4.5, 4.9, 4.6, 3.0, 4.0, 3.7)
cor.test(1:length(defect), defect, method = "spearman")

##Correlation table
#Simplify to make a two-way table describing correlation between pair in a dataframe
library(psycho)
library(corrplot)
data(affective)
view(affective)
c.psy = cor(affective[,c(2,5,6,7,8)])
corrplot(c.psy, method = "number")