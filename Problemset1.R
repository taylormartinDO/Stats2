#Pre
library(tidyverse) 

#Load dataset
ce621 <- read_csv("ce621.csv")

#1)Create a new variable, agegen, to indicate the four age-gender groups.
ce621 = ce621 %>%        # start with the original data, the create subgrups   
  mutate(agegen=case_when(sex=="Male" & age<=60 ~ "m <=60",                        
                          sex=="Female" & age<=60 ~ "f <=60",                        
                          sex=="Male" & age > 60 ~ "m >60",                          
                          sex=="Female" & age > 60 ~ "f >60"))

#2)Inspect the data using side-by-side box plots: 
boxplot(totchg ~ agegen, data=ce621, ylab="Total charges in dollars") 
ce621 %>%   
  group_by(agegen) %>%  
  summarize(obs=n(), mean=mean(totchg), median=median(totchg),  
            sd=sd(totchg), min=min(totchg), max=max(totchg))

#3)Now use the data to perform a linear regression of total charges on the age-gender groups to partition the total variability as displayed in the analysis of variance (ANOVA) table for regression.  The ratio of the regression sum of squares to the error sum of squares provides the Fratio which can be used to test the null hypothesis that the four population means are the same.

model1 = lm(totchg ~ as.factor(agegen), data=ce621) 
anova(model1) 
summary(model1)

#OR
#Estimate (intercept) is the intercept
#Estimate is the slope
#Standard Error

model2 = glm(totchg ~ as.factor(agegen), data=ce621,                                    
             family=gaussian(link="identity")) 
anova(model2) 
summary(model2)



###########################################
#Part B

#Make a boxplot of the residuals by group. Plot the residuals against group.
model1 = lm(totchg ~ as.factor(agegen), data=ce621) 
boxplot(model1$residuals ~ ce621$agegen, ylab="Residuals")

#Plot the residuals against the predicted values.
qplot(x=model1$fitted.values, y=model1$residuals, 
      xlab="Predicted values", ylab="Residuals")

#This graph can be improved by using the jitter option
qplot(x=jitter(model1$fitted.values), y=model1$residuals,  
      xlab="Fitted values", ylab="Residuals")

#One way to address this problem is by analyzing a transformation of the CE expenditure data, rather than the data on its original scale.  This works if you want to ask questions about whether there are differences between groups rather than estimating the size of the differences.  To accomplish this, generate a new variable which is the logarithm(log10)of CE expenditures.
ce621 = ce621 %>%       
  mutate(logtotchg=log10(totchg))
model1 = lm(logtotchg ~ as.factor(agegen), data=ce621) 
boxplot(model1$residuals ~ ce621$agegen, ylab="Residuals")

#####################################################################
#Part C

#Another way to proceed when the focus is the difference in the means themselves, not the means of a transformed value, is to use regression to estimate the means but to use bootstrapping to get more appropriate standard errors that do not depend on the normal and equal variance assumptions.  
library(boot) 

# function to obtain regression coefficients  
bs = function(formula, data, indices) {   
  d = data[indices,] # allows boot to select sample   
  fit = lm(formula, data=d) 
  return(coef(fit)) 
} 

# bootstrapping with 250 replications  
results = boot(data=ce621, statistic=bs, R=250,  formula=totchg~agegen) 

# view results 
results

#Here, we are drawing a random sample with replacement of the original CE cases, doing the regression of totchg on agegen and saving the coefficients. This process is repeated 250 times. The standard errors of the coefficients (group differences) are over the 250 replications as discussed in class.


# get 95% confidence intervals from the bootstrap 
boot.ci(results, type="norm", index=1) # intercept (f <=60) 
boot.ci(results, type="norm", index=2) # f >60 
boot.ci(results, type="norm", index=3) # m <=60 
boot.ci(results, type="norm", index=4) # m >60 

# get 95% confidence intervals from the regression model 
confint(model1)


















