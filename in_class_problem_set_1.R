library(tidyverse)
library(here)
library(ggthemes)
library(GGally)

#BUAN5510
alumni <- read_csv(here("raw_data","Alumni.csv"))
summary(alumni)

alumni %>% ggplot(aes(classeslt20,alumnigivingrate)) + geom_jitter()
alumni %>% ggplot(aes(sfratio,alumnigivingrate)) + geom_jitter()


alumni %>% ggpairs(columns = c("alumnigivingrate","classeslt20","sfratio"))

# The linear approximation appears to make sence in this scenario

#2 - use summarise

mean(alumni$classeslt20)
sd(alumni$classeslt20)
min(alumni$classeslt20)
max(alumni$classeslt20)

mean(alumni$sfratio)
sd(alumni$sfratio)
min(alumni$sfratio)
max(alumni$sfratio)

mean(alumni$alumnigivingrate)
sd(alumni$alumnigivingrate)
min(alumni$alumnigivingrate)
max(alumni$alumnigivingrate)

model_1 <- lm(alumnigivingrate~classeslt20,data = alumni)
summary(model_1)
# The independent variable has a positive slope as expected with the graph; The b1 is statistic
#significant at almost 100% but the H0 for b0 is not statistic significant
#R2 is 42% which means that 42% of the variability of giving rates is explanied by the variability of Classes 
#with less than 20 students

model_2 <- lm(alumnigivingrate~sfratio,data = alumni)
summary(model_2)
# The independent variable has a negative slope as expected with the graph; The b1 is statistic
#significant at almost 100% as well as b0
#R2 is 55% which means that 55% of the variability of giving rates is explanied by the variability of  
#student facult ratio.

#both variables are related to the size of school professors to students, so it is expected that both 
#variables are dependent. The Correl between them is -0.79 

