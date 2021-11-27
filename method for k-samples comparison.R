###K-samples Comparison Methods
##Klotz
#Comparing variance using normal score
#testing whether k samples have equal variance or not 
library(coin)
library(tidyr)
diet.a = c(2.23, 1.14, 2.63, 1.00, 1.35, 2.01, 1.64, 1.13, 1.01, 1.70)
diet.b = c(5.59, 0.96, 6.96, 1.23, 1.61, 2.94, 1.96, 3.68, 1.54, 2.59)
diet.c = c(4.50, 3.92, 10.33, 8.23, 2.07, 4.90, 6.84, 6.42, 3.72, 6.00)
diet.d = c(1.35, 1.06, 0.74, 0.96, 1.16, 2.08, 0.69, 0.68, 0.84, 1.34)
diet.e = c(1.40, 1.51, 2.49, 1.74, 1.59, 1.36, 3.00, 4.81, 5.21, 5.12)
diet = cbind(diet.a, diet.b, diet.c, diet.d, diet.e)
diet = diet %>% gather("diet.type", "iron", 1:5)
diet[,1] = as.factor(diet[,1])
klotz_test(iron~diet.type, diet)
