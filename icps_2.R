# in-class problem set 2

# Data from http://fmwww.bc.edu/ec-p/data/wooldridge/datasets.list.html
# Wooldridge data sets
# WAGE2.DES
# 
# wage      hours     IQ        KWW       educ      exper     tenure    age      
# married   black     south     urban     sibs      brthord   meduc     feduc    
# lwage     
# 
# Obs:   935
# 
# 1. wage                     monthly earnings
# 2. hours                    average weekly hours
# 3. IQ                       IQ score
# 4. KWW                      knowledge of world work score
# 5. educ                     years of education
# 6. exper                    years of work experience
# 7. tenure                   years with current employer
# 8. age                      age in years
# 9. married                  =1 if married
# 10. black                    =1 if black
# 11. south                    =1 if live in south
# 12. urban                    =1 if live in SMSA
# 13. sibs                     number of siblings
# 14. brthord                  birth order
# 15. meduc                    mother's education
# 16. feduc                    father's education
# 17. lwage                    natural log of wage

# Packages
library(tidyverse)
library(here)
library(car)
library(GGally)

# Load data

wages <- read_csv(here("raw_data", "wages.csv"))

df <- wages %>% 
  select(wage, hours, IQ, educ, exper, tenure, age, meduc, feduc)

# Descriptive statistics ----

summary(df)

# Missing information for meduc and feduc
# Little variation in age

# Correlations ----

ggpairs(df) # default is all columns
df %>% select(-meduc, -feduc) %>% 
  cor()
cor(df, use = "na")

# Strongest correlation between 
# - IQ and educ
# - age and experience

# Regression 1 ----

mod_1 <- lm(wage ~ hours + IQ, data = df)
summary(mod_1)

# Regression 2 ----

mod_2 <- lm(wage ~ hours + IQ + educ, data = df)
summary(mod_2)

# Regression 3 ----

mod_3 <- lm(wage ~ hours + IQ + educ + age + exper + tenure, data = df)
summary(mod_3)
  

# Joint hypotheses ----

drop_both <- c("hours = 0", "tenure = 0")
linearHypothesis(mod_3, drop_both)

tenure_eq_exper <- c("tenure = exper")
linearHypothesis(mod_3, tenure_eq_exper)

age_eq_exper <- c("age = exper")
linearHypothesis(mod_3, age_eq_exper)

# Bonus

mod_4 <- lm(wage ~ hours + IQ + educ + age + exper + tenure + meduc + feduc, data = df)
summary(mod_4)

parents_null <- c("meduc = feduc")
linearHypothesis(mod_4, parents_null)

linearHypothesis(mod_4, age_eq_exper)





