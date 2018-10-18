library(tidyverse)
#summarize == summarise; the guy who create the packages created both to represent American and Britsh English
library(here)
library(gridExtra)
library(GGally)

### DATA TREATMENT ######

#2005-2006 National Family and Health Survey from India
nfhs <- read_csv(here("raw_data", "nfhs_3.csv")) 

#hhid-hv208: Household level variables
#hvidx_XX Household roster
#hv108_XX: Education in years
#haX_XX Female roster with age, height, and weight
#hbX_XX Male roster with age, height, and weight

hh <- select(nfhs, hhid:hv208, hv270)


#hhid => Family ID
#hvidx => person ID inside a family
#hv108 => education in years of each person ID inside a family

educ <- nfhs %>%
  select(hhid, starts_with("hvidx"), contains("hv108")) %>%
  gather(variable_name, var_value, -hhid) %>%
  separate(variable_name, c("var", "number"), sep = "_") %>%
  spread(key = var, value = var_value) %>%
  filter(!is.na(hvidx)) %>%
  select(-number) %>%
  rename(roster_id = hvidx, educ = hv108) # Something to merge on!

female <- nfhs %>%
  select(hhid, matches("ha\\d_\\d\\d")) %>%
  gather(variable_name, var_value, -hhid) %>%
  separate(variable_name, c("var", "number"), sep = "_") %>%
  spread(key = var, value = var_value) %>%
  filter(!is.na(ha0)) %>%
  select(-number) %>%
  rename(roster_id = ha0, age = ha1, weight = ha2, height = ha3) %>%
  mutate(female = TRUE)

male <- nfhs %>%
  select(hhid, contains("hb")) %>%
  gather(variable_name, var_value, -hhid) %>%
  separate(variable_name, c("var", "number"), sep = "_") %>%
  spread(key = var, value = var_value) %>%
  filter(!is.na(hb0)) %>%
  select(-number) %>%
  rename(roster_id = hb0, age = hb1, weight = hb2, height = hb3) %>%
  mutate(female = FALSE)

base <- bind_rows(female, male) %>% # Combine male and female
  inner_join(educ) %>% # could also use left here
  inner_join(hh) # note R figures out what to merge on
## Joining, by = c("hhid", "roster_id")
## Joining, by = "hhid"

base <- base %>%
  rename(state = hv024,
         urban_rural = hv025,
         type_place = hv026,
         wealth_index = hv270) %>%
  select(hhid:educ, state, urban_rural,
         type_place, wealth_index) %>%
  mutate(age = as.numeric(age),
         weight = as.numeric(weight),
         height = as.numeric(height),
         educ = as.numeric(educ))
rm(educ, female, hh, male, nfhs)

save(base, file = here("data","base.RData"),  version = NULL, ascii = FALSE, compress = TRUE)



### GRAPH ######
#load Variable if not exists
if (!exists("base")) load(here("data","base.RData"))

# Get descriptitve statistics
summary(base)

# The summary shows that height and Weight has numbers equal to 9999 which doesn't make sense, 
#so we need to replace it for NA. The same think occurs with educ, but in this case the value used was 99
#Also, height and Weight are missing the decimal point, so we need to divide the numbers by 10
base <- base %>%
  mutate(
    weight = case_when(
      weight <= 9000 ~ weight / 10,
      weight > 9000 ~ NA_real_
    ),
    height = case_when(
      height <= 9000 ~ height / 10,
      height > 9000 ~ NA_real_
    ),
    educ = case_when(
      educ <= 90 ~ educ,
      educ > 90 ~ NA_real_
    )
  )

#The age, weight and height looks normal now.
summary(base)

# Make table to show quantitative data by type of place
base %>%
  group_by(type_place) %>%
  summarise(count = n(),
            percent = (sum(count) / nrow(base)) * 100,
            mean_age = mean(age),
            mean_height = mean(height, na.rm = TRUE ),
            mean_weight = mean(weight, na.rm = TRUE),
            female_prop = sum(female)/ sum(count))

# Make table to show quantitative data by female
base %>%
  group_by(female) %>% # group_by() organises data by female
  summarise(count = n(),
            percent = (sum(count) / nrow(base)) * 100,
            mean_age = mean(age),
            mean_height = mean(height, na.rm = TRUE ),
            mean_weight = mean(weight, na.rm = TRUE))

# Make table to show quantitative data by place and female
base %>%
  group_by(type_place, female) %>% # what happens if switch order??
  summarise(count = n(),
            percent = (sum(count) / nrow(base)) * 100,
            mean_age = mean(age),
            mean_height = mean(height, na.rm = TRUE ),
            mean_weight = mean(weight, na.rm = TRUE))

# Use ggplot2 to make a bar plot of place
base %>%
  group_by(type_place) %>% # group_by() groups all data by place
  # aes() sets what will be on the x-axis, use + to add layer
  ggplot(aes(x = type_place)) +
  # geom_bar() determines how data will be arranged, count is default
  geom_bar() +
  ggtitle("Where do people live?") # add title layer
# Loaded gridExtra so can plot more than one graph with grid.arrange
grid.arrange(
  # First graph
  ggplot(base, aes(x = age)) +
    geom_histogram(),
  # Second graph
  ggplot(base, aes(x = educ)) +
    geom_histogram(),
  # Specify number of columns, like using par(mfrow = c(1, 2))
  ncol = 2
)

# If you want to make a boxplot of a single continuous variable you
# have to set x = 1 because the boxplot does not have a width
# associated with it so gives alignment
grid.arrange(
  ggplot(base, aes(x = 1, y = age)) +
    geom_boxplot(),
  ggplot(base, aes(x = 1, y = educ)) +
    geom_boxplot(),
  ncol = 2
)

# addmargins and xtabs are base commands that create
# a crosstab with sum of row and column
addmargins(xtabs( ~ type_place + female, data = base))

grid.arrange(
  base %>%
    count(female, type_place) %>%
    ggplot(aes(x = female, y = type_place)) +
    geom_tile(aes(fill = -n)),
  # geom_tile is used for two categorical variables
  # n tells which attribute to base color on
  # -n the "-" says to go in ascending order
  base %>%
    count(wealth_index, type_place) %>%
    ggplot(aes(x = wealth_index, y = type_place)) +
    geom_tile(aes(fill = -n)),
  ncol = 2
)

cor(base$weight, base$height, use = "complete.obs")

# Load GGally package to get a custom graph of
# correlation not in ggplot2
ggpairs(base, columns = c("age", "weight", "height", "educ"))

ggplot(base, aes(x = height, y = weight)) +
  geom_point() +
  ggtitle("Height vs weight")

base %>% mutate(imc = weight/((height/100)^2)) %>%
    ggpairs(columns = c("age", "educ", "imc", "female", "type_place"))

grid.arrange(
  ggplot(base, aes(x = age, y = height)) +
    geom_point() +
    stat_density2d() +
    ggtitle("Many observations at young ages",
            subtitle = "Contour"),
  ggplot(base, aes(x = age, y = height)) +
    geom_point() +
    stat_density2d(aes(fill = ..density..), geom = "raster",
                   contour = FALSE) +
    ggtitle("Heat map",
            subtitle = "Light color indicates more observations!"),
  ncol = 2
)

# Bar and boxplot
grid.arrange(
  base %>%
    group_by(female) %>%
    summarise(height_mean = mean(height, na.rm = TRUE)) %>%
    ggplot(aes(x = female, y = height_mean, fill = female)) +
    geom_bar(stat = "identity") + # stat="identity" means use the y value as a column
    coord_flip() + # coord_flip() changes bars from verticle to horizontal
    guides(fill = FALSE, ylab = FALSE), # this removes the legend
  base %>%
    group_by(wealth_index) %>%
    ggplot(aes(x = wealth_index, y = educ, fill = wealth_index)) +
    geom_boxplot() +
    coord_flip() +
    guides(fill = FALSE, ylab = FALSE),
  ncol = 2
)
# Basic graph with place by color
base %>%
  count(type_place, female) %>%
  ggplot(aes(x = type_place, y = n, fill = female)) +
  geom_bar(stat = "identity", position = "dodge")

# Basic graph with ethnicity by color
ggplot(base, aes(x = age, y = height, color = female)) +
  geom_point()
ggplot(base, aes(x = age, y = educ,
                 color = type_place,
                 shape = female)) +
  geom_point(size = 3)

######save(base, here("data"))

########## BASICS STATISTICS!!!!!!!