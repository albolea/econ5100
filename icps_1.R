# In-class problem set 1

library(tidyverse)
library(here)

alumni <- read_csv(here("raw_data", "Alumni.csv"))

# Q1 ----
# draw scatter plots of the data, i.e. alumni giving rate vs each of 
# the two other university characteristics (clssslt20 and sfratio)

# Student / faculty ratio
# Without jitter
ggplot(alumni, 
       aes(x = sfratio, y = alumnigivingrate)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x, se = FALSE)+
  xlab("Student / Faculty Ratio") +
  ylab("Percent Alumni who Donate")+
  ggtitle("Relationship between Student/Faculty Ratio and Donations")

ggsave(here("figures",
            "alumni_sfratio.png"))

# With jitter
ggplot(alumni, 
       aes(x = sfratio, y = alumnigivingrate)) +
  geom_jitter() +
  geom_smooth(method = 'lm', formula = y ~ x, se = FALSE)+
  xlab("Student / Faculty Ratio") +
  ylab("Percent Alumni who Donate")+
  ggtitle("Relationship between Student/Faculty Ratio and Donations with Jitter")

ggsave(here("figures",
            "alumni_sfratio_jitter.png"))

# Percent classes with less than 20 students
# Without jitter
ggplot(alumni, 
       aes(x = classeslt20, y = alumnigivingrate)) +
  geom_point() +
  geom_smooth(method = 'lm', formula = y ~ x, se = FALSE)+
  xlab("Student / Faculty Ratio") +
  ylab("Percent Alumni who Donate")+
  ggtitle("Relationship between Percent Classes with less than 20 Students and Donations")

ggsave(here("figures",
            "alumni_classeslt20.png"))

# With jitter
ggplot(alumni, 
       aes(x = classeslt20, y = alumnigivingrate)) +
  geom_jitter() +
  geom_smooth(method = 'lm', formula = y ~ x, se = FALSE) +
  xlab("Student / Faculty Ratio") +
  ylab("Percent Alumni who Donate") +
  ggtitle("Relationship between Percent Classes with less than 20 Students and Donations with Jitter")

ggsave(here("figures",
            "alumni_classeslt20_jitter.png"))

# Q2 -----

# The long version - do this for each variable
alumni %>% 
  summarise(
    mean = mean(alumnigivingrate), sd = sd(alumnigivingrate), min = min(alumnigivingrate), max = max(alumnigivingrate)
  )

# The function version
my_desc_stats <- function(x) {
  x <- enquo(x)
  alumni %>% 
    summarise(
      mean = mean(!! x), 
      sd = sd(!! x), 
      min = min(!! x), 
      max = max(!! x)
      )
}

stat_sfratio <- my_desc_stats(sfratio)
stat_classeslt20 <- my_desc_stats(classeslt20)
stat_alumnigivingrate <- my_desc_stats(alumnigivingrate)
desc_stat <- bind_rows(stat_sfratio, stat_classeslt20, stat_alumnigivingrate)

# The untidy data version - make the data untidy and then group and summarise

grouped <- alumni %>% 
  select(- school) %>% # no sense in doing descriptive stat on this
  gather(key = "var_name") %>% 
  group_by(var_name) %>% 
  summarise(
    mean = mean(value),
    sd   = sd(value),
    min  = min(value),
    max  = max(value)
  )


# Q3 ----

res_q3 <- lm(alumnigivingrate ~ classeslt20, data = alumni)
summary(res_q3)

alumni %>% ggplot(aes(classeslt20,alumnigivingrate)) + geom_point()

# Q4 ----

res_q4 <- lm(alumnigivingrate ~ sfratio, data = alumni)
summary(res_q4)


