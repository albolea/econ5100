library(tidyverse)
library(GGally)
library(here)
library(car)
library(ggthemes)

wages <- read_csv(here("raw_data","wages.csv"))

wages <- wages %>% select(wage,hours,IQ,educ,exper,tenure,age,meduc,feduc)


#1 - Mother Education and Father Education have a great number of NA's
summary(wages)

ggpairs(data = wages)


#2 - Wages and Hours are not correlated
#   Wages and experience are not correlated
cor(wages,use = "complete.obs")

#3 - The intercept is 190.38 and is significant for a level of confidence of 90%
#    b1(hours) is -1.82 but is not significant  for a level of confidence of 90% 
#    b2(IQ) is 8.37 and is  significant for a level of confidence of 90%
model_1 <- lm(wage ~ hours + IQ, data = wages) 
summary(model_1) 

#4 - The intercept is -31.13 but is not significant for a level of confidence of 90%
#    b1(hours) is -2.51 but is not significant  for a level of confidence of 90%
#    b2(IQ) is 5.18 and is  significant for a level of confidence of 90%
#    b3(educ) is 42.65 and is  significant for a level of confidence of 90%

model_2 <- lm(wage ~ hours + IQ + educ, data = wages) 
summary(model_2) 

#5 - the changes in the coeficients show us the new variable is correlated with the two previews variables
# I expected a very low change in the b1(hours) given that hours and education has a correlation of 0.091 
# but the change was very expressive.
#

#6 - Almost all the coefficients of the new model are statistical significant (b1 is the only one that is not)
# and the R2 adjuste improved from 0.13 to 0.17
#Hours continues to present a negative coefficient which appears counterintuitive.
model_3 <- lm(wage ~ hours + IQ + educ + age + exper + tenure, data = wages) 
summary(model_3) 

#7 - 


#8 - The F-test show us the model is statistic significant and the R2-adjusted is very low (0.17)

#9 - With a certain of 95% we can reject the null hypoteses, so Tenure and hours shouldn't be used.

Hnull <- c("tenure = 0", "hours = 0")
linearHypothesis(model_3,Hnull)

#10 - 

Hnull <- c("tenure = 0", "hours = 0")
linearHypothesis(model_3,Hnull)