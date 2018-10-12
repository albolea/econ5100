install.packages("tidyverse")
install.packages("pdftools")
library(tidyverse)
library(pdftools)

text <-read_delim(file = "Z:/GIT/Assignment1.txt",delim=" ",col_names = FALSE,
                  col_types = cols(.default = col_number()) )

text

risk_free <-t(text[1,])
sp500 <-t(text[2,])
apple <-t(text[3,])
intel <- t(text[4,])
safeway <- t(text[5,])

text2 <- read_delim(file = "Z:/GIT/Assignment1_2.txt",delim=" ",col_names = FALSE )



data <- as.tibble(cbind(risk_free,sp500,apple,intel,safeway))%>% 
  rename(risk_free=V1,SP500=V2,apple=V3,intel=V4,safeway=V5)

data %>% ggplot(aes(sp500,apple)) + geom_point() +geom_abline()

fit_apple <- lm(sp500 ~ apple, data = data)
summary(fit_apple)



