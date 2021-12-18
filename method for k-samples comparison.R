###K-samples Comparison Methods
##Klotz
#Comparing variance using normal score
#testing whether k samples have equal variance or not 
library(coin)
library(tidyr)
library(tidyverse)
library(ggpubr)
library(rstatix)
diet.a = c(2.23, 1.14, 2.63, 1.00, 1.35, 2.01, 1.64, 1.13, 1.01, 1.70)
diet.b = c(5.59, 0.96, 6.96, 1.23, 1.61, 2.94, 1.96, 3.68, 1.54, 2.59)
diet.c = c(4.50, 3.92, 10.33, 8.23, 2.07, 4.90, 6.84, 6.42, 3.72, 6.00)
diet.d = c(1.35, 1.06, 0.74, 0.96, 1.16, 2.08, 0.69, 0.68, 0.84, 1.34)
diet.e = c(1.40, 1.51, 2.49, 1.74, 1.59, 1.36, 3.00, 4.81, 5.21, 5.12)
diet = as.data.frame(cbind(diet.a, diet.b, diet.c, diet.d, diet.e))
diet = diet %>% gather("diet.type", "iron", 1:5)
diet[,1] = as.factor(diet[,1])
klotz_test(iron~diet.type, diet)

##Quade
#Test of comparison, a nonparametric alternative to ANOVA
#also equivalent to an extension of wilcoxon signed-rank test(equal when there are only 2 groups)
#used in a complete block design analysis
hospital = c(letters[1:7])
winter = c(92, 9, 98, 19, 21, 58, 42)
spring = c(112,11, 109, 25, 22, 71, 49)
summer = c(94, 10, 92, 19, 23, 51, 44)
fall = c(77, 12, 81, 18, 24,62, 41)
birth = as.data.frame(cbind(hospital, winter, spring, summer, fall))
seasonal = birth[,2:5] %>% gather("season", "num.birth", 1:4)
hospital = rep(birth$hospital, 4)
birth.2 = cbind(hospital, seasonal)
birth.2[,1] = as.factor(birth.2[,1])
birth.2[,2] = as.factor(birth.2[,2])
birth.2[,3] = as.numeric(birth.2[,3])
str(birth.2)
quade.test(num.birth ~ season|hospital, data = birth.2)

y <- matrix(c( 5,  4,  7, 10, 12,
               1,  3,  1,  0,  2,
               16, 12, 22, 22, 35,
               5,  4,  3,  5,  4,
               10,  9,  7, 13, 10,
               19, 18, 28, 37, 58,
               10,  7,  6,  8,  7),
            nrow = 7, byrow = TRUE,
            dimnames =
              list(Store = as.character(1:7),
                   Brand = LETTERS[1:5]))
str(dy <- as.data.frame(as.table(y)))
quade.test(Freq~Brand|Store, data = dy)

##Friedman 
#similar to quade, alternative to ANOVA
pot.a = c(746, 768, 721)
pot.b = c(717, 757, 780)
pot.c = c(776, 773, 774)
pot.d = c(814, 815, 787)
pot.e = c(763, 800, 793)
pot = as.data.frame(cbind(pot.a, pot.b,pot.c,pot.d,pot.e))
pot = pot%>%gather("level", "strength", 1:5)
block = rep(c("a", "b","c"),5)
pot = cbind(block, pot)
pot = pot%>% convert_as_factor(block, level)
friedman_test(pot, strength~block|level)
friedman.test(strength~block|level, data = pot)