#Stat Team 6
#Load Packages

library(openintro)
library(dplyr)
library(tidyr)
library(infer)
library(ggplot2)

# Prediction using model from part 3 --------------------------------------
# What would life expectancy be in a South American country in the year 6000?

pred <- linear_model_prediction(life_year_mod, 6000)

# Test: is this significantly lower from the average?

test_stat <- numeric_z(
  x = pred,
  mu = mean(s_amer$life_exp),
  s = sd(s_amer$life_exp)
)

pnorm(test_stat, lower.tail = TRUE)

# Since the p-value is greater than 0.05, we fail to reject the null
# hypothesis. A South American country in the year 6000
# will not have an unusually low life expectancy.

# Proportion net users of a country with rest of world -------------------
# Is the proportion in Mexico with internet users over 1000 in the year 2015 
# significantly different.
# than the proportion of internet users with the rest of the globe?

group_test_data <- project_data %>% 
  filter(year == 2015 & !is.na(net_users)) %>%
  mutate(is_greater_1000 = 
           case_when(
             net_users >= 1000 ~ 1,
             net_users < 1000 ~ 0
           ),
         samp =
           case_when(
             country == "Mexico" ~ "Mexico",
             TRUE ~ "Global"
           )
  ) %>% 
  select(samp, is_greater_1000) %>% 
  group_by(samp) %>% 
  summarize(count = n(),
            users_greater_1000 = sum(is_greater_1000)/count)

mexico_users <- group_test_data %>% 
  filter(samp == "Mexico")

global_users <- group_test_data %>% 
  filter(samp != "Mexico")

test_stat_2 <- categorical_z(
  p_hat = mexico_users$users_greater_1000,
  p = global_users$users_greater_1000,
  n = mexico_users$count
)

pnorm(test_stat_2, lower.tail = FALSE)

# The p-value is greater than 0.05 so we fail to reject the null hypothesis.
## The proportion of net users in Mexico with net users above 1000 
# in the year 2015 is significantly different from the rest of the globe.