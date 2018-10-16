library(tidyverse)
library(ggthemes)
armands_pizza <- tibble(
                    restaurant = seq(1:10),
                    student_pop = c(2,6,8,8,12,16,20,20,22,26),
                    qrt_sales = c(58,105,88,118,117,137,157,169,149,202)
)

armands_pizza %>% ggplot(aes(student_pop,qrt_sales)) + theme_economist()+geom_point()+geom_smooth(method = "lm")

x_bar <-mean(armands_pizza$student_pop)
y_bar <- mean(armands_pizza$qrt_sales)
b1 <- sum((armands_pizza$student_pop-x_bar)*(armands_pizza$qrt_sales-y_bar)) /
  sum((armands_pizza$student_pop-x_bar)^2)
b0 <- y_bar - b1*x_bar

armands_pizza %>% ggplot(aes(student_pop,qrt_sales)) + theme_economist()+geom_point()+
  geom_abline(intercept = b0,slope = b1)

armands_pizza <- armands_pizza %>% mutate(y_hat = b0+b1*student_pop)
sse <-  sum((armands_pizza$qrt_sales - armands_pizza$y_hat)^2)
sst <- sum((armands_pizza$qrt_sales - y_bar)^2)
ssr <- sum((armands_pizza$y_hat - y_bar)^2)
r_two <- ssr/sst
correl <- sqrt(r_two)

fit <- lm(qrt_sales ~student_pop,data = armands_pizza)
fit
