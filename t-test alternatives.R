###t-test alternatives
install.packages("EcoSimR_0.1.0.tar.gz", repos = NULL, type = "source")
library(tidyverse)
library(rstatix)
library(ggpubr)
library(datarium)
library(reshape2)
library(EcoSimR)
library(ggrepel)

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
#to help on understanding randomization test we are about to use, lets try simulating "fake data"
#from a known distribution
set.seed(12)
fakedata = rnorm(100, 0, 1)#simulate data with 100 observations from normal(0,1) distribution
plot_fake = ggplot(data.frame(fakedata), aes(x=fakedata)) +
  geom_histogram(color="black", fill="white") +
  ylab("Count") +
  geom_vline(xintercept = mean(fakedata), linetype = "dashed", 
             size = 1, colour = "red")
plot_text = plot_fake + 
  annotate("text", x = -0.30, y = 9, label = round(mean(fakedata), 4)) +
  annotate("text", x = -0.30, y = 9.5, label = "Mean")
plot_text
#note that the mean is not equal to the distribution parameter mean
#this gives a part of sampling illustration
#now suppose the sampling from above is repeated 1000 times, while each time calculating the mean
set.seed(123)
fakedata1 = replicate(1000, mean(rnorm(100, 0, 1)))
plot_fake2 = ggplot(data.frame(fakedata1), aes(x=fakedata1)) +
  geom_histogram(color="black", fill="white") +
  ylab("Count") +
  geom_vline(xintercept = mean(fakedata1), linetype = "dashed", 
             size = 1, colour = "red")
plot_text2 = plot_fake2 + 
  annotate("text", x = -0.30, y = 9, label = round(mean(fakedata1), 4)) +
  annotate("text", x = -0.30, y = 14, label = "Mean")
plot_text2
#as it can be seen, the means distribution from our resampling procedures looks alot closer
#to that of normal distribution plot.

#what have we discussed on the simulation above is pretty much representing the randomization test.
#in summary the randomization test is done by : 
#1)calculate test statistics(can be anything from means to difference in mean, or a specific test statistics such as t)
#2)reshuffle the observations, while each time calculating the test statistics
#3)repeat (2) several times generating our "distribution" of the test statistics
#4)calculate the proportion of times the actual test statistic is outside the distribution of test-statistics
#to illustrate this, lets yet again do simulation
#Imagine we had ventured to South America and collected 10 male and 10 female Hercules beetles.
#We brought the beetles back to the lab and measured the width of their bodies. 
#The question we are interested in : Do male and female Hercules beetles differ in body width?
set.seed(1234)
df_males = data.frame(width = rnorm(10, mean=16.5, sd=2.5),
  sex = "Male")#male data
df_females = data.frame(width = rnorm(n=10, mean=15, sd=1.8),
  sex = "Female")#female data
df_body_widths = rbind(df_males, df_females)#combine both female and male data
#plot the data
ggplot(df_body_widths, aes(x = width, fill = sex)) +
  ylab("Count") + xlab("Body width") +
  geom_histogram(bins = 30, colour = "black") +
  geom_vline(data = filter(df_body_widths, sex == "Male"), aes(xintercept = mean(width)),
             size = 1, linetype = "dashed", colour = "red") +
  geom_vline(data = filter(df_body_widths, sex == "Female"), aes(xintercept = mean(width)),
             size = 1, linetype = "dashed", colour = "blue")
mean_males = mean(df_males$width)
mean_females = mean(df_females$width)
diff_means_obs = mean_males - mean_females
#test the difference in body width with t test
t.test(width ~ sex, data = df_body_widths, alternative = "two.sided")

#then start the randomization test by reshuffling our observations
set.seed(12321)
reshuffled = df_body_widths
reshuffled$width = sample(reshuffled$width, 
                           size = nrow(reshuffled), replace = FALSE)
ggplot(reshuffled, aes(x = width, fill = sex)) +
  ylab("Count") + xlab("Body width") +
  geom_histogram(bins = 30, colour = "black") +
  geom_vline(data = filter(reshuffled, sex == "Male"), aes(xintercept = mean(width)), 
             size = 1, linetype = "dashed", colour = "red") +
  geom_vline(data = filter(reshuffled, sex == "Female"),aes(xintercept = mean(width)), 
             size = 1, linetype = "dashed", colour = "blue")
#calculate our statistics : difference in mean of width between male and females beetles
mean_males_sim1 = mean(reshuffled %>% filter(sex == "Male") 
                        %>% pull(width))
mean_females_sim1 = mean(reshuffled %>% filter(sex == "Female") 
                          %>% pull(width))
mean_diff_sim1 = mean_males_sim1 - mean_females_sim1
#repeat these reshuffling 1000 times
set.seed(11)

simulated_means = list()
for(i in 1:1000){
  #Create temporary dataframe to permute so we don't modify the original
  reshuffled = df_body_widths 
  #Permute the width column with the 'sample()' function. 
  reshuffled$width = sample(reshuffled$width, size = nrow(reshuffled), 
                             replace = FALSE)
  #Calculate the means for each sex
  mean_males_sim = mean(reshuffled %>% filter(sex == "Male") 
                         %>% pull(width))
  mean_females_sim = mean(reshuffled %>% filter(sex == "Female") 
                           %>% pull(width))
  #Calculate to difference between simulated male and female body width means
  mean_diff_sim = mean_males_sim - mean_females_sim
  simulated_means[i] = mean_diff_sim
}    
simulated_means = unlist(simulated_means)
#plot the result from reshuffling
ggplot() +
  ylab("Count") + xlab("Simulated mean difference") +
  geom_histogram(aes(x = simulated_means), bins = 30, 
                 fill = "grey", alpha = 0.4, colour = "black") +
  geom_vline(xintercept = diff_means_obs, size = 1, 
             linetype = "dashed", colour = "black")
abs_simulated_means = abs(simulated_means)
abs_diff_means_obs = abs(diff_means_obs)
exceed_count = length(abs_simulated_means[abs_simulated_means >= 
                                             abs_diff_means_obs])
p_val = exceed_count / 1000
#note that the decision in randomization is the same as the previous t test

#real life illustration : ecologist's interest in understanding the factors that shape the organization
#of ecological communities. Are the species in an ecological community just a random assortment of species
#available from the regional species pool? Conversely, do species interactions and shared resources 
#determine the local distribution of species, such that some species are found together more often 
#than we would expect by chance whereas other never or only rarely co-occur. 
#The idea that competition and shared resources are important in driving community assembly 
#is known as assembly rules and was first proposed by Diamond (1975).
#The question we are interested in addressing is: 
#Are the observed species co-occurrence patterns the result of species interactions or random chance?
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
#For our index, we will use the C-score (Stone and Roberts 1990). 
#We will not go into the calculation of this score here but suffice it to say that 
#the higher the C-score, the less co-occurrence (on average) between all pairs of species 
#in the matrix (i.e. a more segregated matrix). 
set.seed(50)

co_oc_analysis <- cooc_null_model(PA_matrix, algo = "sim2", 
                                  nReps=1000, 
                                  metric = "c_score", 
                                  suppressProg = TRUE, 
                                  saveSeed = TRUE)


#summarize output from co-occurrence analysis
summary(co_oc_analysis)
plot(co_oc_analysis)
#our red line fall way below than where we would expect if it is a random chance.
#this suggests the community is aggregated such that some species are more likely to occur 
#with other species in the community than we would expect if the communities were 
#assembling themselves randomly. 
#this can happen if there is facilitation and some species create ecological niches or favourable habitat for other species such that they occur more often. 