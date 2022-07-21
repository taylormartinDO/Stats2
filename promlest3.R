library(tidyverse)
nepal621 = read_csv("nepal621_v2.csv")

#Create a table that displays the numbers of deaths and numbers of survivors for the vitamin A and control groups separately for the six age-by-sex strata.  Include totals across all strata and percentages so that the reader of your table can see the evidence for whether or not vitamin A is beneficial
nepal621 %>%    group_by(trt, sex, age) %>%   
  summarize(N_Alive = sum(status=="Alive"), 
            Perc_Alive = N_Alive/n(),          
            N_Died = sum(status=="Died"),          
            Perc_Died = N_Died/n(),        
            Total=n())

# Make it a little prettier! 
nepal621 %>%   
  group_by(trt, sex, age) %>%   
  summarize(N_Alive = sum(status=="Alive"),     
            Perc_Alive = round(N_Alive/n(),4)*100,           
            N_Died = sum(status=="Died"),           
            Perc_Died = round(N_Died/n(),4)*100,       
            Total=n())

#estimate the proportion of children who died in the vitamin A group and in the control group and estimate the difference in mortality rates between the two groups.
nepal621 %>%  
  group_by(trt) %>%   
  summarize(N_Alive = sum(status=="Alive"),         
            Perc_Alive = round(N_Alive/n(),4)*100,           
            N_Died = sum(status=="Died"),            
            Perc_Died = round(N_Died/n(),4)*100,      
            Total=n())

#95% confidence interval for each true mortality rate.
nepal621 %>%  
  group_by(trt) %>%   
  summarize(N_Alive = sum(status=="Alive"),      
            p_Alive = N_Alive/n(),           
            N_Died = sum(status=="Died"),          
            p_Died = N_Died/n(),         
            Total = n(),           
            se_Died = sqrt(p_Died *(1-p_Died)/Total),            
            CI_L = p_Died - 1.96*se_Died,          
            CI_U = p_Died + 1.96*se_Died)

#Calculations
#placebo
0.0217-1.96*0.00126

#Vit A
0.0170-1.96*0.00110
0.0170+1.96*0.00110


#
p.1 = 0.217   # fill in sample proportion for first sample
  n.1 = 133389    # fill in sample size for first sample 
  p.2 =  .17  # fill in sample proportion for second sample 
  n.2 = 13732    # fill in sample size for second sample 
  
diff = p.1 - p.2 
# standard error
se = sqrt(p.1*(1-p.1)/n.1 + p.2*(1-p.2)/n.2)    
# confidence interval
diff - 1.96*se; diff + 1.96*se   


#95% confidence interval for the difference in mortality rates for the vitamin A and control groups separately for each age-sex stratum
nepal621 %>%    
  group_by(sex, age, trt) %>%    
  summarize(N_Died = sum(status=="Died"),             
            p_Died = N_Died/n(),             
            Total = n())

#F<1
p.1 = 0.0536   # fill in sample proportion for first sample
n.1 = 1288    # fill in sample size for first sample 
p.2 =  0.0401  # fill in sample proportion for second sample 
n.2 = 1345    # fill in sample size for second sample 

diff = p.1 - p.2 
# standard error
se = sqrt(p.1*(1-p.1)/n.1 + p.2*(1-p.2)/n.2)    
# confidence interval
diff - 1.96*se; diff + 1.96*se 


#F1-2
p.1 = 0.0268   # fill in sample proportion for first sample
n.1 = 2687    # fill in sample size for first sample 
p.2 =  0.0187  # fill in sample proportion for second sample 
n.2 = 2776    # fill in sample size for second sample 

diff = p.1 - p.2 
# standard error
se = sqrt(p.1*(1-p.1)/n.1 + p.2*(1-p.2)/n.2)    
# confidence interval
diff - 1.96*se; diff + 1.96*se 


#F3-4
p.1 = 0.00974   # fill in sample proportion for first sample
n.1 = 2567    # fill in sample size for first sample 
p.2 =  0.00590  # fill in sample proportion for second sample 
n.2 = 2544    # fill in sample size for second sample 

diff = p.1 - p.2 
# standard error
se = sqrt(p.1*(1-p.1)/n.1 + p.2*(1-p.2)/n.2)    
# confidence interval
diff - 1.96*se; diff + 1.96*se 

#M<1
p.1 = 0.0384   # fill in sample proportion for first sample
n.1 = 1327    # fill in sample size for first sample 
p.2 =  0.0421  # fill in sample proportion for second sample 
n.2 = 1426    # fill in sample size for second sample 

diff = p.1 - p.2 
# standard error
se = sqrt(p.1*(1-p.1)/n.1 + p.2*(1-p.2)/n.2)    
# confidence interval
diff - 1.96*se; diff + 1.96*se 

#M1-2
p.1 = 0.0167   # fill in sample proportion for first sample
n.1 = 2817    # fill in sample size for first sample 
p.2 =  0.0139  # fill in sample proportion for second sample 
n.2 = 2877    # fill in sample size for second sample 

diff = p.1 - p.2 
# standard error
se = sqrt(p.1*(1-p.1)/n.1 + p.2*(1-p.2)/n.2)    
# confidence interval
diff - 1.96*se; diff + 1.96*se 


#M3-4
p.1 = 0.00962   # fill in sample proportion for first sample
n.1 = 2703    # fill in sample size for first sample 
p.2 =  0.00434  # fill in sample proportion for second sample 
n.2 = 2764    # fill in sample size for second sample 

diff = p.1 - p.2 
# standard error
se = sqrt(p.1*(1-p.1)/n.1 + p.2*(1-p.2)/n.2)    
# confidence interval
diff - 1.96*se; diff + 1.96*se 

#double check
nepal621 %>%   
  group_by(sex, age) %>%  
  summarize(N_Plac = sum(trt=="Placebo"),   
            p_Plac = sum(status=="Died" & trt=="Placebo")/N_Plac,  
            N_VitA = sum(trt=="Vit A"),         
            p_VitA = sum(status=="Died" & trt=="Vit A")/N_VitA,      
            diff = p_Plac - p_VitA,          
            se = sqrt(p_Plac*(1 - p_Plac)/N_Plac + p_VitA*(1 - p_VitA)/N_VitA),       
            CI_L = diff - 1.96*se,   
            CI_U = diff + 1.96*se) 


#graph
library(Hmisc) 

dataForCIplot = nepal621 %>%    
  group_by(sex, age) %>%  
  summarize(N_Plac = sum(trt=="Placebo"),       
            p_Plac = sum(status=="Died" & trt=="Placebo")/N_Plac,     
            N_VitA = sum(trt=="Vit A"),        
            p_VitA = sum(status=="Died" & trt=="Vit A")/N_VitA,       
            diff = p_Plac - p_VitA,        
            se = sqrt(p_Plac*(1 - p_Plac)/N_Plac + p_VitA*(1 - p_VitA)/N_VitA),      
            CI_L = diff - 1.96*se,       
            CI_U = diff + 1.96*se) 

agestrata = c(1,2,3,4,5,6,7) 
agestrata_labels = c("F < 1", "F 1-2", "F 3-4", "M < 1", "M 1-2", "M 3-4", "Overall") 
diff = c(dataForCIplot$diff, 0.0047) 
LL = c(dataForCIplot$CI_L, 0.00142) 
UL = c(dataForCIplot$CI_U, 0.00798) 

## Add labels to the axes 
errbar(x = agestrata,   
       y = diff,    
       yplus = LL,   
       yminus = UL,   
       xaxt = "n",         #xaxt removes the numberic lables       
       xlab = "Age/Gender Group",      #label for x-axis       
       ylab = "Difference in Mortality Rates (VitA - Placebo)")  #label for y-axis()

## Add a title 
title(main="95% Confidence Intervals for Difference in Mortality Rates") 

## Add group labels for the age-gender groups 
axis(side=1,                 #1 = the bottom of graph     
     at=agestrata,              #where on x-axis; same as "x" in errbar   
     labels=agestrata_labels)   #what the labels are

# Add horizontal line at zero 
abline(h=0, col="red")


model1 = glm(as.factor(status) ~ trt, data=nepal621, family=binomial(link="identity")) 
summary(model1)
confint(model1)




















