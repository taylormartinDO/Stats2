

library(tidyverse)
nepalibf = read_csv("nepalibf.csv")
head(nepalibf) 

#examine all the variables.  You can also table the continuous variables independently
summary(nepalibf) 
table(nepalibf$bf)

#Compare boxplots for the continuous covariates (X’s) vs breastfeeding status
boxplot(age_chld ~ bf, data=nepalibf, ylab="Age of child (months)") 
boxplot(age_mom ~ bf, data=nepalibf) 
boxplot(parity ~ bf, data=nepalibf) 

#Tabulate the proportion breastfed by gender
CT = xtabs(~ sex_chld + bf, data=nepalibf) 
addmargins(CT) 
prop.table(CT, margin=1) 
addmargins(prop.table(CT, margin=1), margin=2)

#Estimate the prevalence of breast feeding as a function of child’s age (centered at the mean age) and gender using logistic regression:
nepalibf = nepalibf %>% 
  mutate(agechldc = age_chld - mean(age_chld))

model1 = glm(bf ~ sex_chld + agechldc, data=nepalibf, 
             family=binomial(link="logit"))

summary(model1) 
exp(model1$coefficients) 
exp(confint.default(model1)) 

#Test the hypothesis that the association of breast feeding prevalence and child’s age is  different for boys and girls (interaction model)
modelE = glm(bf~agechldc+sex_chld+sex_chld:agechldc, data=nepalibf,  
             family=binomial(link="logit")) 
summary(modelE) 

#remove the missing data from the bfvariable (if you haven’t already) and then recode sex-chld to be a factor variable.
#Display a graph of the estimated breast feeding prevalence -vs- child’s age with separate curves for boys and girls
nepalibf = nepalibf %>%   
  na.omit() %>%  # Remove observations with missing data  
  mutate(sex_chld=recode_factor(sex_chld, `0`="Male", `1`="Female")) # Factor sex_chld  

modelD = glm(bf ~ sex_chld + age_chld, data=nepalibf, family=binomial(link="logit")) 
nepalibf = nepalibf %>% mutate(phat = predict(modelD, type="response"))

qplot(x=age_chld, y=phat, color=sex_chld, shape=sex_chld, data=nepalibf, 
      xlab="Child's age in months", ylab="Predicted prevalence of Breast-feeding")

#Hosmer-Lemeshow goodness-of- fit test for the model that includes gender and child’s age.  Divide the predicted probabilities into 10 roughly equal groups for calculation of the chi-square comparing observed and expected events across the groups

#install.packages("ResourceSelection")  # only if haven't installed yet 
library(ResourceSelection) 

hoslem.test(nepalibf$bf, nepalibf$phat, g=10) 
hoslem.test(nepalibf$bf, nepalibf$phat)$observed 
hoslem.test(nepalibf$bf, nepalibf$phat)$expected 








