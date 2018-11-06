# Regression diagnostics
# Mainly done through simulations

library(tidyverse)
library(here)

# Nothing wrong - base model ----

set.seed(2)
x <- runif(400) * 100
error <- rnorm(400, 0, 25)
df <- data.frame(x, error) %>% 
  mutate(y = 1.5 * x + error + 40)

df %>% ggplot(aes(x = error)) + geom_histogram( bins = 25)
ggsave(here("figures", "diag_nw_errors.png"))

mod_1 <- lm(y ~ x, data = df)
summary(mod_1)

df %>% ggplot(aes(x = x, y = y)) + geom_point() +
  geom_smooth(method = "lm", se = F)
ggsave(here("figures", "diag_nw_reg_line.png"))

# Standardised residuals plot
df <- df %>% 
  mutate(
    stand_res = rstandard(mod_1)
  )
df %>% ggplot(aes(x = stand_res)) +
  geom_histogram() + xlab("Standardized residuals")
ggsave(here("figures", "diag_nw_stand_res.png"))

# Constant variance
df$fitted <- mod_1$fitted.values
df$residuals <- mod_1$residuals
df %>% ggplot(aes(x = fitted, y = residuals)) +
  geom_point()
ggsave(here("figures", "diag_nw_homoskedasticity.png"))

# Non-normal errors ----

set.seed(4)
df$chi2_error <- rchisq(400, 4) * 10
df <- df %>% 
  mutate(y_non_normal = 1.5 * x + chi2_error)

mean(df$chi2_error)
df %>% ggplot(aes(x = chi2_error)) +
  geom_histogram(bins = 25)
ggsave(here("figures", "diag_chi2_errors.png"))

mod_2 <- lm(y_non_normal ~ x, data = df)
summary(mod_2)

df %>% ggplot(aes(x = x, y = y_non_normal)) + geom_point() +
  geom_smooth(method = "lm", se = F)
ggsave(here("figures", "diag_chi2_reg_line.png"))

# Standardised residuals plot
df <- df %>% 
  mutate(
    stand_res_chi2 = rstandard(mod_2)
  )
df %>% ggplot(aes(x = stand_res_chi2)) +
  geom_histogram(bins = 25) + xlab("Standardized residuals")
ggsave(here("figures", "diag_chi2_stand_res.png"))

# Constant variance
df$fitted_chi2 <- mod_2$fitted.values
df$residuals_chi2 <- mod_2$residuals
df %>% ggplot(aes(x = fitted_chi2, y = residuals_chi2)) +
  geom_point()
ggsave(here("figures", "diag_chi2_homoskedasticity.png"))

# Non-constant variance ----

df <- df %>% 
  mutate(
    hetero_error = error * x/20,
    y_hetero = 40 + 1.5 * x + hetero_error
  )

df %>% ggplot(aes(x = hetero_error)) +
  geom_histogram(bins = 25)

mod_3 <- lm(y_hetero ~ x, data = df)
summary(mod_3)

df %>% ggplot(aes(x = x, y = y_hetero)) + geom_point() +
  geom_smooth(method = "lm", se = F)
ggsave(here("figures", "diag_hetero_reg_line.png"))

# Standardised residuals plot
df <- df %>% 
  mutate(
    stand_res_hetero = rstandard(mod_3)
  )
df %>% ggplot(aes(x = stand_res_hetero)) +
  geom_histogram(bins = 25) + xlab("Standardized residuals")
ggsave(here("figures", "diag_hetero_stand_res.png"))

# Constant variance
df$fitted_hetero <- mod_3$fitted.values
df$residuals_hetero <- mod_3$residuals
df %>% ggplot(aes(x = fitted_hetero, y = residuals_hetero)) +
  geom_point()
ggsave(here("figures", "diag_hetero_homoskedasticity.png"))


# Non-linear model ----

df <- df %>% 
  mutate(
    y_non_linear = 40 + 1.5 * x^1.7 / 10 + error
  )

mod_4 <- lm(y_non_linear ~ x, data = df)
summary(mod_4)

df %>% ggplot(aes(x = x, y = y_non_linear)) + geom_point() +
  geom_smooth(method = "lm", se = F)
ggsave(here("figures", "diag_non_linear_reg_line.png"))

# Standardised residuals plot
df <- df %>% 
  mutate(
    stand_res_non_linear = rstandard(mod_4)
  )
df %>% ggplot(aes(x = stand_res_non_linear)) +
  geom_histogram(bins = 25) + xlab("Standardized residuals")
ggsave(here("figures", "diag_non_linear_stand_res.png"))

# Constant variance
df$fitted_non_linear <- mod_4$fitted.values
df$residuals_non_linear <- mod_4$residuals
df %>% ggplot(aes(x = fitted_non_linear, y = residuals_non_linear)) +
  geom_point()
ggsave(here("figures", "diag_non_linear_homoskedasticity.png"))

