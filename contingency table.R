### Contingency table ###
##chi square test
#test used in : dependency of 2 categorical variables, homogenity, and goodness of fit
#suppose 3 porfessors are about to compare their grading policies, is there significant
#evidence that at least 2 out of the 3 professors' grading policies are different
library(tidyr)
library(tidyverse)
library(ggpubr)
library(rstatix)
grading = matrix(c(12, 45, 49, 6, 13, 18, 2,
                   10, 32, 43, 18, 4, 12, 6, 
                   15, 19, 32, 20, 6, 9, 7), byrow = T, nrow = 3,
                 dimnames =
                   list(professor = c("smith", "jones", "white")))
chisq.test(grading)
#there is strong evidence that at least 2 of the professors' grading policies are different

##Kruskal-Wallis
#extension of Mann-Whitney U test, when there are more than 2 samples
#See : Conover W. J. (1999), p. 288-290
#KW test can also be applied in a contingency table
#for example, lets use grading policies of the previous data
#same question, but KW test will be used
grading = as.data.frame(t(grading))
grading = grading %>% gather("professor", "num", 1:3)
grading[,1] = as.factor(grading[,1])
kruskal_test(num~professor, data = grading)
