# Title: "Project Part IV Individual Portion"
# Date: 11/30/21
# Author: Joana Hadzhilazova
# Variables used: continent(Africa), num_phones, year

# Load Packages:
library(dplyr)
library(infer)
library(stat242)
library(moderndive)

# Load Data From Project Part III:
source("Users/valeriailieva/Desktop/Fall 2021/Math 242/StatProject1/Hadzhilazova_lm.R")

# Refer to the Data Set Used in this Project:
View(project_data)

# Model from Part III:
africa <- project_data %>% 
  filter(continent == "Africa" & num_phones > 0)

the_mod <- lm(formula = num_phones ~ year, 
              data = africa)

## This is the model showing year v num_phones

# Prediction Using Model From Part III:
## What is the number of phones (num_phones) in a country in Africa in the year 2001

pred <- linear_model_prediction(the_mod, 2001)

# Conducting the Test and Finding the p-value:
test_stat <- numeric_z(
  x = pred,
  mu = mean(africa$num_phones),
  s = sd(africa$num_phones)
)
pnorm(test_stat, lower.tail = TRUE)

# The p-value is approximately 0.4015 which is greater than the significance level (alpha = 0.05), therefore we fail to reject the null hypothesis.
# An African country during the year 2001, will not have an unusually low number of phones.

# Categorical Hypothesis:
## Is the proportion of countries in Africa with more than 1500000 phones, higher that the proportion in the other continents (the rest of globe)?

cat_test_data <- project_data %>% 
  filter(!is.na(num_phones))%>%
  mutate(more_than_1500000 = 
           case_when(
             num_phones > 1500000 ~ 1,
             num_phones <= 1500000 ~ 0  #a few countries happen to have exactly 1500000 phones
           ),
         samp =
           case_when(
             continent == "Africa" ~ "Africa",
             TRUE ~ "Other Continents"
           )
  ) %>% 
  select(samp, more_than_1500000) %>% 
  group_by(samp) %>% 
  summarize(count = n(),
            proportion_more_than_1500000 = sum(more_than_1500000)/count)

africa_proportion <- cat_test_data %>% 
  filter(samp == "Africa")

other_continents_proportion <- cat_test_data %>% 
  filter(samp != "Africa")

test_stat_2 <- categorical_z(
  p_hat = africa_proportion$proportion_more_than_1500000,
  p = other_continents_proportion$proportion_more_than_1500000,
  n = africa_proportion$count
)

pnorm(test_stat_2, lower.tail = FALSE)  

# The p-value is 1, which is more than the significance level (alpha = 0.05), therefore we fail to reject the null hypothesis.
# The proportion of countries in Africa with a number of phones that is greater than 1500000 is significantly different from the proportion of the rest of the globe.
