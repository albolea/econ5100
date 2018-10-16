library(tidyverse)
library(dslabs)
library(ggthemes)
library(HistData)
library(here)
data("GaltonFamilies")

galton_heights <- GaltonFamilies %>% filter(childNum == 1 & gender == "male") %>% 
  select(father,childHeight) %>% rename(son = childHeight)

galton_heights %>% ggplot(aes(father,son)) + geom_point()

galton_heights2 <- GaltonFamilies  %>% filter( gender == "male") %>% 
  select(father,childHeight) %>% rename(son = childHeight)

lm(son ~ father, data=galton_heights)


alumni <- read_csv(here("raw_data", "Alumni.csv") )
summary(alumni)


fit <- lm(alumnigivingrate ~ sfratio, data=alumni)
summary(fit)
