library(tidyverse)
library(here)

baseball <- read_csv(here("raw_data","baseball.csv"))

summary(baseball)

cor(baseball$Wins,baseball$Payroll.millions)

model <- lm(Wins ~ Payroll.millions,data=baseball)
summary(model)

b1 <- as.numeric(model$coefficients[2])
b0 <- as.numeric(model$coefficients[1])

y_hat <- round(b0 + b1*88,0)

y_2_hat <- round(b0 + b1*225,0)

summary(model)
