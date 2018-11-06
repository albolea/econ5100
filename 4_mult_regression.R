library(tidyverse)
library(here)
library(ggthemes)
library(GGally)

#BUAN5510
load(here("data","LPGA.RData"))

LPGA <- LPGA %>% mutate(earnings.usd.million = earnings.usd/(10^6))
summary(LPGA)

fit1 <- lm(earnings.usd ~ greens.in.reg,data=LPGA)
fit1b <- lm(earnings.usd.million ~ greens.in.reg,data=LPGA)

fit2 <- lm(earnings.usd ~ putting.avg,data=LPGA)
fit2b <- lm(earnings.usd.million ~ putting.avg,data=LPGA)

fit3 <- lm(earnings.usd ~greens.in.reg + putting.avg ,data=LPGA)
fit3b <- lm(earnings.usd.million ~greens.in.reg + putting.avg ,data=LPGA)

r_adj = tibble(dollar = c(
                        summary(fit1)$adj.r.squared,
                        summary(fit2)$adj.r.squared,
                        summary(fit3)$adj.r.squared ), 
               million = c(
                        summary(fit1b)$adj.r.squared,
                        summary(fit2b)$adj.r.squared,
                        summary(fit3b)$adj.r.squared ) )
r_adj

?mtcars #Motor Trend Car Road Tests



car_ur <- lm(mpg ~ cyl + disp + hp + wt, data = mtcars)
summary(car_ur)
car_ur_summary <- summary(car_ur)
#the variables are not independent significant, but are they jointly significant ?

cor(mtcars$disp,mtcars$hp)

#Restricted Model
car_r <- lm(mpg ~ cyl + wt, data= mtcars)

summary(car_r)

#The R-squared version
r2_ur <- summary(car_ur)$r.squared
r2_r <- summary(car_r)$r.squared

q <- length(car_ur$coefficients) - length(car_r$coefficients) 
n_k_1 <- (length(car_r$residuals) - length(car_ur$coefficients))
F_value <- ((r2_ur - r2_r)/q)/((1-r2_ur)*n_k_1)
qf(0.9,df= q, df2= n_k_1) #Critical Value for 10% sig level

### Function Version
#install.packages("car")
library(car) #Regression Analysis Packge
Hnull <- c("disp = 0", "hp = 0")
linearHypothesis(car_ur,Hnull)

