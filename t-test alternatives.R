###t-test alternatives
library(tidyverse)
library(rstatix)
library(ggpubr)
library(datarium)
library(reshape2)
library(ecosim)
install.packages('EcoSimR')

##wilcoxon test
#recommended to apply on data that is not satisfying the normality assumption for t test
#just like t-test, wilcoxon test also has several types. lets take a look at mice dataset again
data(mice2)
mice2
mice2 = mice2 %>%
  gather(key = "group", value = "weight", before, after) 
mice2 %>% group_by(group) %>% summarise(n = length(weight),  
                                        mean_weight = mean(weight),
                                        median_weight = median(weight),
                                        iqr = IQR(weight))
#one sample : testing whther the median of the mice weight is 23g
bxp = ggboxplot(
  mice$weight, width = 0.5, add = c("mean", "jitter"), 
  ylab = "Weight (g)", xlab = FALSE
)
bxp
wilcox.test(mice$weight, mu = 23)
#two sample : testing between 2 groups, whether there is a significance difference between two medians
#using genderweight dataset from datarium
data("genderweight", package = "datarium")
genderweight %>% group_by(group) %>% summarise(n = length(weight),  
                                               mean_weight = mean(weight),
                                               median_weight = median(weight),
                                               iqr = IQR(weight))
bxp2 = ggboxplot(
  genderweight, x = "group", y = "weight", xlab = "Gender", ylab = "weight", 
  width = 0.5, add = c("mean", "jitter"), 
)
bxp2
wilcox.test(genderweight$weight ~ genderweight$group)
#signed rank on paired sample 
data("mice2")
miceff = mice2 %>% gather(key = "group", value = "weight", before, after)
wilcox.test(miceff$weight ~ miceff$group, paired = TRUE)

##randomization test
#randomization tests use functions to randomly shuffle data.
#to evaluate mean differences, after each shuffle, the means computed from the shuffled data are 
#compared with the observed mean difference.
#illustration : ecologist's interest in understanding the factors that shape the organization
#of ecological communities. Are the species in an ecological community just a random assortment of species
#available from the regional species pool? Conversely, do species interactions and shared resources 
#determine the local distribution of species, such that some species are found together more often 
#than we would expect by chance whereas other never or only rarely co-occur. 
#The idea that competition and shared resources are important in driving community assembly 
#is known as assembly rules and was first proposed by Diamond (1975). 
#lets look at the actual application, starting with the data
neon_data = "https://uoftcoders.github.io/rcourse/data/NEON_PlantPA_HARV_201707.csv"
download.file(neon_data, "NEON_PlantPA_HARV_201707.csv")
neon_data = read_csv("NEON_PlantPA_HARV_201707.csv")
#data are from the National Ecological Observatory Network and represent the presence and absence 
#of plant species in eight square meter plots during the month of July, 2017, at a single site 
#around Harvard, Boston, MA
#change into presence-absence matrix
neon_data_filtered = neon_data %>%
  # We only want rows with plant species and not 'otherVariables'
  dplyr::filter(divDataType == "plantSpecies") %>% 
  
  # To create a presence-absence matrix, we only need the taxonID 
  # (i.e. species) and the plotID (i.e. Site)
  dplyr::select(plotID, taxonID) 

#only keep unique rows so the cells represent presence-absence and not abundances
neon_data_filtered = unique(neon_data_filtered) 
PA_matrix = dcast(neon_data_filtered, formula = taxonID ~ plotID, fun.aggregate = length)
head(PA_matrix)
#cooccurence analysis
set.seed(50)

co_oc_analysis <- cooc_null_model(PA_matrix, algo = "sim2", 
                                  nReps=1000, 
                                  metric = "c_score", 
                                  suppressProg = TRUE, 
                                  saveSeed = TRUE)


# Summarize output from co-occurrence analysis
summary(co_oc_analysis)