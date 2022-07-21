library(tidyverse) 
nepal621 = read_csv("nepal621_v2.csv")

#Create two age groups (< 3 years, ≥ 3 years). Estimate the odds ratio with a 95% confidence interval for vitamin A exposure by vital status within each age stratum. Test the null hypothesis that vitamin A is not effective for each age group
nepal621 = nepal621 %>%  
  mutate(agegp = ifelse(age == "3 to 4", "3+ years", "<3 years"))

# Calculates the odds by age group and trt; can the find CI by hand 
nepal621 %>%   
  group_by(agegp, trt) %>%   
  summarize(N_Alive = sum(status=="Alive"),      
            N_Died = sum(status=="Died"),           
            Odds = N_Died/N_Alive)

# Let R do all the calculations for you! 
nepal621 %>%   
  group_by(agegp) %>%  
  summarize(N_Alive_P = sum(status=="Alive" & trt=="Placebo"),          
            N_Died_P = sum(status=="Died" & trt=="Placebo"),        
            N_Alive_V = sum(status=="Alive" & trt=="Vit A"),      
            N_Died_V = sum(status=="Died" & trt=="Vit A"),          
            OR = (N_Died_V/N_Alive_V)/(N_Died_P/N_Alive_P),           
            se = sqrt(1/N_Alive_P + 1/N_Died_P + 1/N_Alive_V + 1/N_Died_V),   
            CI_L = exp(log(OR)-1.96*se),    
            CI_U = exp(log(OR)+1.96*se))

#odds ratio by using a logistic regression of the binary survival indicator on vitamin A
nepal621.lowage = nepal621 %>% filter(agegp == "<3 years") 
model2 = glm(as.factor(status) ~ trt, data=nepal621.lowage,  family=binomial(link="logit")) 

summary(model2)  # This summary is on the logOR scale 
exp(model2$coefficients)  # We exponentiate to get on the OR scale 
exp(confint(model2))  # We can also exponentiate the CI to the OR scale 

nepal621.highage = nepal621 %>% filter(agegp == "3+ years") 
model3 = glm(as.factor(status) ~ trt, data=nepal621.highage,  
             family=binomial(link="logit"))

summary(model3) 
exp(model3$coefficients) 
exp(confint(model3)) 




































