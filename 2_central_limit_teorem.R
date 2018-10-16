library(tidyverse)
library(here)

### load Base

base %>% summarise(avg_edu = mean(educ, na.rm=T))

set.seed(2)

sample_size <- 5

get_mean <- function(df,sample_size){
  df <- sample_n(df,sample_size,replace = TRUE) %>%
    summarise(avg_edu = mean(educ, na.rm=TRUE))
  df$avg_edu
}
B <- 1000
sample_of_avg <- replicate(B, get_mean(base,sample_size))

hist(sample_of_avg)
mean(sample_of_avg)

#Given the sample size is 5 we got a very lage sigma

sample_size <- 50
sample_of_avg <- replicate(B, get_mean(base,sample_size))

hist(sample_of_avg)
mean(sample_of_avg)

sample_size <- 500
sample_of_avg <- replicate(B, get_mean(base,sample_size))

hist(sample_of_avg)
mean(sample_of_avg)

sample_size <- 5000
sample_of_avg <- replicate(B, get_mean(base,sample_size))

hist(sample_of_avg)
mean(sample_of_avg)

##### Try to do this as a smaller code #####