##### Problem Set 1 - ECON 5100 - Renato Albolea - 10/18/2018 #####

library(tidyverse)
library(here)
library(ggthemes)

base <- read_csv(here("raw_data","baseball.csv"))

head(base)
summary(base)

avg_payroll <- round(mean(base$Payroll.millions),0)
sd_payroll <- round(sd(base$Payroll.millions),0)
cor_payroll_wins <- round(cor(base$Payroll.millions,base$Wins),2)

base %>% ggplot(aes(Payroll.millions,Wins,label = Team)) + geom_point() + 
  geom_label(data=subset(base,Payroll.millions >120 ),nudge_y = -2) +
  theme_economist() + scale_y_continuous(breaks = seq(60,105,5))

base %>% arrange(desc(Payroll.millions)) %>% head()

base %>% arrange(desc(Wins)) %>% head()

test <- base %>% filter(Payroll.millions <150) 

cor_payroll_wins2 <- round(cor(test$Payroll.millions,test$Wins),2)
