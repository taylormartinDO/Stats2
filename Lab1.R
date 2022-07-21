#Pre
library(tidyverse) 

#Load dataset
lab1data <- read_csv("44133-Lab 1 Data.csv")

#Plot
ggplot(lab1data, aes(x=year)) +   
  ylab("Percent with no mammagram") +   
  geom_point(aes(y=Maryland, col="Maryland")) +   
  geom_point(aes(y=US, col="US")) +   
  scale_colour_manual("", breaks = c("Maryland", "US"),  
      values = c("red", "blue"))

#Summary
summary(lab1data)

#Standard Deviation of Year (by year?)
sd(lab1data$year)
#Standard Deviation of US popuation
sd(lab1data$US)
#SD of Maryland population
sd(lab1data$Maryland)

#Correlation between year and % of no mammogram in US population
cor(lab1data$year, lab1data$US)


#Sets model
model1 = lm(US ~ year, data=lab1data)

#Summary of model
summary(model1)

#ANOVA
anova(model1) 

#95% Confidence Interval
confint(model1)

################################################

#Center the data
lab1data = lab1data %>% mutate(yearcen = year - 1995)
# Assign model3 to it
model3 = lm(US ~ yearcen, data=lab1data)
# Summary of model3
summary(model3)
#ANOVA of model3
anova(model3)
#95% CI
confint(model3)


################################

#The following ggplot() allows us to plot multiple points and regression lines on a single plot.  The command geom_point() adds points to the graph and the command geom_smooth() adds the fitted values from the regression model.  In each case, the x variable is year (specified in the first line with ggplot(), but the y variable changes for the different points/lines.

#Linear Model
ggplot(lab1data, aes(x=year)) +   
  ylab("Percent with no mammagram") +  
  geom_point(aes(y=Maryland, col="Maryland")) +   
  geom_smooth(aes(y=Maryland, col="Maryland"), method="lm", se=FALSE) +  
  geom_point(aes(y=US, col="US")) + 
  geom_smooth(aes(y=US, col="US"), method="lm", se=FALSE) +  
  scale_colour_manual("",  
      breaks = c("Maryland", "US"), values = c("red", "blue"))

#Generalized Linear Model (instead of linear model)
model5 = glm(US ~ yearcen, data=lab1data, family=gaussian(link="identity")) 
summary(model5)

confint(model5)
