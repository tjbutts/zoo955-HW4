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

barplot(log(variance_spo$variance), ylab = 'log(spore density variance)', xlab = 'hive') # Took log for visualization purposes # 
box()

# Visually variance's look heterogeneous but let's check statistically - can run a Levene's Test to confirm 
library(car)
result = leveneTest(y=dat$Spobee, group=dat$Hive)
print(result) # Non-significant so statistically variance of spore density by Hive has homogeneous variance 


#=========== Question 1 ANSWER ====================================================# 
# The variance here is statistically homogenous but still looks pretty heterogeneous. When visualizing the variances, 
# there is quite a bit of variation between the hives. This is likely due to differences in which hives are infected 
# and the degree of infection between the hives causing differences in variance of spore density. 
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

#==================QUESTION 3=====================================#
# Develop a simple linear model for transformed spore density. Include infection, number of bees, and their interaction as 
# explanatory variables. Check for hive effect by plotting standardized residuals against hive ID. 

library(lme4)
#library(nlme)

# create a variable indicating whether they are infected


beeLM <- lm(log(Spobee+0.1)~Infection * BeesN, data = dat)

beeRes = residuals(beeLM, type = 'pearson')

plot(beeRes~dat$Hive, xlab = 'Hive', ylab = 'Residuals')

#===============================================================================#
#==============QUESTION 3 ANSWER================================================#
# The residuals do not seem homogenous among hives; they actually seem to vary a
# good amount.
#===============================================================================#

#===============QUESTION 4================================================================#
# What are the advantages of including hive as a random effect, 
# rather than as a fixed effect?
#===============================================================================#


#==============QUESTION 4 ANSWER================================================#
# If we apply hive as a random effect we are able to better control for
# differences in model fit among hives. This is also useful to control for
# non-independence among hives
#===============================================================================#

#===============================================================================#
# Q5. Step 3. Choose a variance structure or structures (the random 
 # effects). What random effects do you want to try?
#==============QUESTION 5 ANSWER================================================#
# We want to try hive as a random effect because each individual hive is only
# one in a population of hives, and that way we can better control for differences
# in model residuals across hive.
#===============================================================================#


dat$sporeTrans <- log(dat$Spobee+0.1)


#==================QUESTION 6===============================================================#
# Q6. Step 4. Fit the "beyond optimal" ME model(s) with lmer() in the 
#lme4 package (transformed spore density is response, fInfection01, 
             # sBeesN, and interaction are the explanatory variables). Show your code.
#==============QUESTION 6 ANSWER================================================#
beeLME <- lmer(sporeTrans~Infection* log(BeesN) +(1|Hive), data = dat, REML = TRUE)
#===============================================================================#


#==============QUESTION 7==========================#
#Compare the linear regression and ME model(s) with a 
#likelihood ratio test, including correction for testing on the boundary 
#if needed. Use the anova() command. This will re-fit your lmer model 
#with maximum likelihood, but this is OK (note there are some debates 
 #                                        about exactly how to best compare an lm and lmer model). Show your 
#work and the results. Which random effect structure do you choose 
#based on the results?

summary(beeLME)

# plot the residuals
plot(residuals(beeLME, type = "pearson"))


library(lmtest)
# compare the two models using likelihood ratio test
lrtest(beeLM, beeLME)

#=================================================================================#
#==============QUESTION 7 ANSWER================================================#
# The full model with the random effect is better than the simple linear model because
# p << 0.0001
#===============================================================================#

#==================QUESTION 8===================================#
#Check the model: plot standardized residuals vs. fitted 
#values and vs. each predictor. (You can get standardized residuals 
#with residuals(yourmodel, type='pearson')). How do they look?
#=======================================================#
fitted(beeLM)
fitted(beeLME)


FLME <- fitted(beeLME) # get the residuals for the linear model
RLME <- residuals(beeLME)

plot(RLME~FLME, ylab = "residuals", xlab = "fitted")


#=================================================================================#
#==============QUESTION 8 ANSWER================================================#
plot(RLME~dat$sporeTrans) 
plot(RLME~dat$BeesN)
plot(RLME~dat$Hive)
#=================================================================================#

#===========================Question 9================# 
#Step 7. Re-fit the full model with ML (set REML=FALSE) and 
#compare against a reduced model without the interaction term, also fit 
#with ML. Use anova() to compare the models. Which model do you 
#choose? Why?
#======================================================#

beeLME <- lmer(sporeTrans~Infection* log(BeesN) +(1|Hive), data = dat, REML = TRUE)

#=================================================================================#
#==============QUESTION 9 ANSWER================================================#
beeLME_REMLFalse_noInter <- lmer(sporeTrans~Infection+ log(BeesN) +(1|Hive), data = dat, REML = FALSE)
anova(beeLME, beeLME_REMLFalse_noInter)

# The model is not improved by dropping an interaction term because the p-value = 0.78
#=================================================================================#



#================QUESTION 10=========================#
#Q10. Step 8. Iterate #7 to arrive at the final model. Show your work. 
#What is your final set of fixed effects?
#====================================================#

#=================================================================================#
#==============QUESTION 10 ANSWER================================================#
beeLME <- lmer(sporeTrans~Infection* log(BeesN) +(1|Hive), data = dat, REML = FALSE)
beeLME_REMLFalse_noInter <- lmer(sporeTrans~Infection+ log(BeesN) +(1|Hive), data = dat, REML = FALSE)
beeLME_REMLFalse <- lmer(sporeTrans~Infection* log(BeesN) +(1|Hive), data = dat, REML = FALSE)
beeLME_REMLFalse_onlyInter <- lmer(sporeTrans~Infection: log(BeesN) +(1|Hive), data = dat, REML = FALSE)

# tried one additional model with only the interaction term
AIC(beeLME, beeLME_REMLFalse_noInter, beeLME_REMLFalse, beeLME_REMLFalse_onlyInter)

#                            df      AIC
#beeLME                      6 294.0506
#beeLME_REMLFalse_noInter    5 292.1286
#beeLME_REMLFalse            6 294.0506
#beeLME_REMLFalse_onlyInter  5 292.2628

# The model without an interaction term between Infection and the number 
# of bees (transformed) is most supported by AIC. Because delta AIC between the
# top model and the other models is <2, they are not statistically different
# models and we would need to consider all in analysis. However, if we were to fit 
# these models with REML the results would likely be different. Our final set of
# fixed effects was infection and the log-transformed number of bees without an interaction term
#=================================================================================#

#==================QUESTION 11=============================#
#Step 9. Fit the final model with REML. Check assumptions by 
#plotting a histogram of residuals, plotting Pearson standardized 
#residuals vs. fitted values, and plotting Pearson standardized 
#residuals vs. explanatory variables. Are there issues with the model? 
 # If so, how might you address them?
#=============================================================================#
#==============QUESTION 11 ANSWER================================================#
beeLME_REML_TRUE_noInter <- lmer(sporeTrans~Infection+ log(BeesN) +(1|Hive), data = dat, REML = TRUE)

plot(hist(residuals(beeLME_REML_TRUE_noInter)))
# residuals look normally distributed

plot(residuals(beeLME_REML_TRUE_noInter, type = "pearson")~fitted(beeLME_REML_TRUE_noInter))

# Pretty evenly distributed!

# plot versus each of the explanatory variables
plot(residuals(beeLME_REML_TRUE_noInter, type = "pearson")~dat$Hive)
plot(residuals(beeLME_REML_TRUE_noInter, type = "pearson")~log(dat$BeesN))


# The model seems to work pretty well across hives (which is likely because of the random effect)
# but does not seem to work well across the number of bees. A random effect could be created for 
# categories of bee density, or there could just be a lot of variation in the number of bees
# per hive
#=============================================================================#

#====================QUESTION 12=========================#
## Q12. Step 10. Interpret the model. The summary() command is useful 
## here. What have you learned about American Foulbrood? 
#=============================================================================#

#==============QUESTION 12 ANSWER================================================#
summary(beeLME_REML_TRUE_noInter)
anova(beeLME_REML_TRUE_noInter)

# The density of spores per bee increases with the number of infected bees and with
# the number of bees in the hive. This is likely because a greater number of bees
# the disease can spread more easily. The model also explains approximately 82% of the variation
# in spore density from the random effect of hives alone, which according to the model
# is related to bee density.

totalVar = 5.693 +1.262
varExp = 5.693/totalVar

#=============================================================================#

#=================QUESTION 13================================# 
#Calculate the correlation between observations from the same 
#hive as variance(fhive random effect)/(variance(fhive random effect) + 
#                                         variance(residual)). Given the correlation among observations from 
#the same hive, do you think it's a good use of time to sample each 
#hive multiple times? Why or why not?


#=============================================================================#
#==============QUESTION 13 ANSWER================================================#
#print(VarCorr(beeLME_REML_TRUE_noInter), comp = "Variance")

#Groups   Name        Variance
#Hive     (Intercept) 5.6929  
#Residual             1.2624  

#correlation = 5.6929/(5.6929+1.2624)

# Because the correlation is so high (0.82) we don't gain
# much new information from sampling the same hive multiple times

