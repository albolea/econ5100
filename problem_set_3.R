library(tidyverse)
library(GGally)
library(car)
library(here)

#load(url("https://www.dropbox.com/s/wn20qtlf5qjs65w/econ5100_ps3.RData?dl=0"))
load(here("data","econ5100_ps3.RData"))
  #st.fips (State FIPS Code)
  #name (State or District)
  #year (Year),
  #unempr (State Unemployment Rate, measured as a rate, that is between 0 and 1, rather than in percent)
  #i.fsp.tot (Number of FSP Individuals),
  #pop (Population),
  #fsp.rate (State FSP Participation Rate, measured as a rate, that is between 0 and 1).

summary(snap)

model1 <- lm(fsp.rate ~ unempr, data = snap)
summary(model1)

Y_hat = model1$coefficients[1]+model1$coefficients[2]*0.08
