# Load packages -----------------------------------------------------------

library(dplyr)
library(infer)
library(stat242)

# Load data from part 3 ---------------------------------------------------

source(here::here("ha_lm.R"))

# Prediction using model from part 3 --------------------------------------
# What would life expectancy be in an Asian country with 6000 net users?

pred <- linear_model_prediction(life_net_users_mod, 6000)

# Test: is this significantly lower from the average?

test_stat <- numeric_z(
  x = pred,
  mu = mean(asia$life_exp),
  s = sd(asia$life_exp)
)

pnorm(test_stat, lower.tail = TRUE)

# Since the p-value is greater than 0.05, we fail to reject the null
# hypothesis. A European country with a gdp per capita of 6000
# will not have an unusually low life expectancy.

# Categorical Hypothesis Test ---------------------------------------------
# Is the proportion of European countries with a life expectancy above 60
# greater than the proportion of the rest of the globe? Here, we'll consider
# the other continents as one group and conduct a two-sample test. 

cat_test_data <- project_data %>% 
  mutate(is_greater_60 = 
           case_when(
             life_exp > 60 ~ 1,
             life_exp < 60 ~ 0
           ),
         samp =
           case_when(
             continent == "Asia" ~ "Asia",
             TRUE ~ "Global"
           )
  ) %>% 
  select(samp, is_greater_60) %>% 
  group_by(samp) %>% 
  summarize(count = n(),
            prop_greater_60 = sum(is_greater_60)/count)

asia_prop <- cat_test_data %>% 
  filter(samp == "Asia")

global_prop <- cat_test_data %>% 
  filter(samp != "Asia")

test_stat_2 <- categorical_z(
  p_hat = asia_prop$prop_greater_60,
  p = global_prop$prop_greater_60,
  n = asia_prop$count
)

pnorm(test_stat_2, lower.tail = FALSE)  

# Since the p-value is nearly 0, we reject the null hypothesis. The proportion
# of European countries with a life expectancy above 60 is significantly 
# different from the rest of the globe.

