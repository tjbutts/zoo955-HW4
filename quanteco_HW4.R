## Arbitrary time cut-off points = survival analysis ## 

# Mixed Models # 
library(tidyverse)
library(here)

dat = read_csv(here('data', 'Bees.csv')) %>%
  mutate(Hive = as.factor(Hive)) %>%
  mutate(Infection = if_else(Infection > 0, 'infected', 'clean'))
dat

# Bee Metadata 
# Spobee = density of p larvae spores per bee 
# Hive = Categorical variable of hive ID (change into a factor)
# Infection = degree of infection -> can be turned into a yes or no 

# We will model the density of P. larvae with the potential 
# explanatory variables as number of bees in the hive, presence or absence 
# of AFB, and hive identity. 

#===================================================================================#
# Q1 -  Does variance of spore density appear homogenous among hives? Why or why not? 
#===================================================================================# 

# First going to calculate variance # 

variance_spo = dat %>%
  group_by(Hive) %>%
  summarize(
    variance = var(Spobee)) %>%
  ungroup()
variance_spo

barplot(log(variance_spo$variance), ylab = 'log(variance)', xlab = 'hive') # Took log for visualization purposes # 
box()

# Visually variance's look heterogeneous but let's check statistically - can run a Levene's Test to confirm 
library(car)
result = leveneTest(y=dat$Spobee, group=dat$Hive)
print(result) # Non-significant so statistically variance of spore density by Hive has homogeneous variance 


#=========== Question 1 ANSWER ====================================================# 
# The variance here is statistically homogenous but still looks pretty hetergeneous 
#============ STILL NEED TO FIGURE OUT THE WHY ==================# 
# (Why or Why not) --The reason here I am not sure maybe related to infection?-- 
#=================================================================================#

# Q2 - Try some transformations of the response variable to homogenize 
# the variances (or at least improve it). Which transformation of spore 
# density seems reasonable? Why?

# Creat some transformations # 
# Log transformation, sqrt transformation, arcSine transformation, Reciprocal transformation # 

# First check out data to see skew in a few hives # 
dat_transforms = dat %>% 
  mutate(log_trans = log(Spobee), 
         sqrt_trans = sqrt(Spobee), 
         cubert_trans = Spobee^1/3)

# Check to see how variance looks by hive now 
variance_trans = dat_transforms %>%
  group_by(Hive) %>%
  summarize(
    variance_log = var(log_trans), 
    variance_sqrt = var(sqrt_trans), 
    variance_cubert = var(cubert_trans)) %>%
  ungroup()
variance_trans

# Log transform # - better 
barplot(variance_trans$variance_log, ylab = 'variance', xlab = 'hive') # Took log for visualization purposes # 
box()

# Sqrt transform # - not good 
barplot(variance_trans$variance_sqrt, ylab = 'variance', xlab = 'hive') # Took log for visualization purposes # 
box()

# Cuberoot transform # - worse 
barplot(variance_trans$variance_cubert, ylab = 'variance', xlab = 'hive') # Took log for visualization purposes # 
box()

# Visually assess the data, a log transform improves homegeneity of variance among the data 
boxplot(log(Spobee+0.1) ~ Hive, # add 0.1 to remove NA values # 
        data = dat,
        xlab = "Hive",
        ylab = "Spobee",
        col = "steelblue",
        border = "black")

result2 = leveneTest(y=log(dat$Spobee+0.1), group=dat$Hive)
print(result2) # Transformed Levene Test - larger p-value so homogeneity of variance improved 
print(result) # Initial Levene Test 

#==============================================================================# 
#============QUESTION 2 ANSWER=================================================# 
# The log transformation is typically used to make variances among groups more homogenous and it did a far better job 
# then the other transformations here. Looking at the Levene test results the log transformation increased the p-value so 
# the variances were even more statistically homogenous 
#===============================================================================# 

# develop a simple linear model for transformed spore density

library(lme4)
#library(nlme)

# create a variable indicating whether they are infected


beeModel <- lm(log(Spobee+0.1)~Infection * BeesN, data = dat)

beeRes = residuals(beeModel, type = 'pearson')

plot(beeRes~dat$Hive)

#===============================================================================#
#==============QUESTION 3 ANSWER================================================#
# The residuals do not seem homogenous among hives; they actually seem to vary a
# good amount.
#===============================================================================#

#===============================================================================#
#==============QUESTION 4 ANSWER================================================#
# If we apply hive as a random effect we are able to better control for
# differences in model fit among hives. This is also useful to control for
# non-independence among hives
#===============================================================================#

#===============================================================================#
#==============QUESTION 5 ANSWER================================================#
# We want to try hive as a random effect because each individual hive is only
# one in a population of hives, and that way we can better control for differences
# in model residuals across hive.
#===============================================================================#


dat$sporeTrans <- log(dat$Spobee+0.1)

beeLME <- lmer(sporeTrans~Infection*BeesN +1|Hive, data = dat)
beeLME <- lme(sporeTrans~Infection*BeesN +(1|Hive), data = dat)

