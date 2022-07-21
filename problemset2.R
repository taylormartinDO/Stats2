#Pre
library(tidyverse) 

#Load dataset
nepalA <- read_csv("nepal_anthro.csv")

nepalData = nepalA %>%   ### store this dataset under a new name   
filter(age <= 12) %>%   ### keep only children with age <= 12   
filter(!is.na(height), !is.na(weight), !is.na(armcirc)) ### remove NAs

nepalData = nepalData %>%    
  mutate(gender = recode_factor(sex, `1`="Male", `2`="Female"))
### the first level you designate will be the reference! 

qplot(x=jitter(age), y=weight, color=gender, shape=gender,  
      data=nepalData, xlab="Age in months", ylab="Weight in kg")

model1 = lm(weight ~ age + gender, data=nepalData) 
summary(model1) 
confint(model1) 

model2 = lm(weight ~ age*gender, data=nepalData) ## This is equivalent to lm(weight ~ age + gender + age:gender)
summary(model2) 
confint(model2)

qplot(x=jitter(age), y=weight, color=gender, shape=gender, data=nepalData,  
      xlab="Age in months", ylab="Weight in kg") +          
  geom_line(aes(x = age, y=model2$fitted.values, color=gender))

#Calculate the residuals from this model and plot them against age using the same colors for boys and girls as were used for the raw data. Comment whether this model, that assumes growth is linear in the first year, is adequate.
qplot(y=model2$residuals, x=jitter(age), color=gender, shape=gender,  
      data=nepalData, ylab="Residuals", xlab="Age in months")

#Add a linear spline term with a break at 4 months and its interaction with gender to the model above. Call the new model B. Calculate and plot the fitted values from Model B against age. Interpret each of the coefficients in Model B
nepalData = nepalData %>% mutate(agesp = ifelse(age > 4, age-4, 0)) 

model3 = lm(weight ~ age*gender + agesp*gender, data=nepalData) 
summary(model3) 
confint(model3)

qplot(x=jitter(age), y=weight, color=gender, shape=gender, 
      data=nepalData, xlab="Age in months", ylab="Weight in kg") +    
  geom_line(aes(x = age, y=model3$fitted.values, color=gender))

#Test the null hypothesis that growth is linear over the first twelve months by testing whether the coefficients of the two new terms in Model B are both zero
anova(model2, model3)  # tests the coefficients that are different between the two models

#Another way to do it
library(survey) 
regTermTest(model3, ~ agesp + agesp:gender)

#Calculate the residuals from this model and plot them against age. Comment on any assumptions of the linear regression that still appear to be violated.
qplot(y=model3$residuals, x=jitter(age), data=nepalData, ylab="Residuals", 
      xlab="Age in months") + geom_hline(yintercept=0, color="red")


#############################################################################
#Plot weight against age (use small plotting symbols, e.g. dots, and jitter the points so they can all be seen).   
qplot(x=jitter(age), y=weight, data=nepalData, xlab="Age in months",  
      ylab="Weight in kg", ylim=c(0,12))

nepalData %>% group_by(age) %>%summarize(mean=mean(weight))

qplot(x=jitter(age), y=weight, data=nepalData, xlab="Age in months",  
      ylab="Weight in kg", ylim=c(0,12)) +      
      stat_summary(aes(x=age, y=weight), fun.y=mean, geom="line", lwd=2,    
      color="red")

#Use simple linear regression to regress weight on age and add the least squares line to the plot. 
model4 = lm(weight ~ age, data=nepalData) 
summary(model4) 
confint(model4) 


qplot(x=jitter(age), y=weight, data=nepalData, xlab="Age in months",  
       ylab="Weight in kg", ylim=c(0,12)) + geom_smooth(method="lm")


#Calculate the residuals from the simple linear regression above and plot them versus age. Add a smooth function using lowess smoothing.
qplot(y=model4$residuals, x=jitter(age), data=nepalData, ylab="Residuals",  
      xlab="Age in months") +   
  geom_smooth(method="loess", se=FALSE) +      #loess smoother 
  geom_hline(yintercept=0, color="red")        #horizontal y=0 line

#regress weight onto the monthly mean ages. 
model5 = lm(weight ~ as.factor(age), data=nepalData) 
summary(model5) 
confint(model5)

#
#age_sp1 = (age – 2)+ = age – 2 if age > 2, 0 if not 
#age_sp2 = (age - 4)+ = age – 4 if age > 4, 0 if not
#age_sp3 = (age - 6)+ = age – 6 if age > 6, 0 if not 

nepalData = nepalData %>%    
  mutate(age_sp1 = ifelse(age > 2, age-2, 0)) %>%   
  mutate(age_sp2 = ifelse(age > 4, age-4, 0)) %>%   
  mutate(age_sp3 = ifelse(age > 6, age-6, 0))

#Regress weight on age, age_sp1, age_sp2 and age_sp3. 
model6 = lm(weight ~ age + age_sp1 + age_sp2 + age_sp3, data=nepalData) 
summary(model6) 
confint(model6)

#
qplot(x=jitter(age), y=weight, data=nepalData, xlab="Age in months",  
  ylab="Weight in kg", ylim=c(0,12)) +      
  geom_line(aes(x = age, y=model6$fitted.values), color="red", lwd=2)

#Ftest
anova(model4, model6)

AIC(model4, model5, model6)

mean(model4$residuals^2)

mean(model5$residuals^2)

mean(model6$residuals^2)





















































