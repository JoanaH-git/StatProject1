# Load packages -----------------------------------------------------------

library(dplyr)
library(infer)
library(stat242)

# Load data from part 3 ---------------------------------------------------

source(here::here("aguirre_lm.R"))
project_data <- stat242::project_data

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

# Categorical Hypothesis Test ---------------------------------------------
# Is the proportion of South American countries with a life expectancy above 
# 60 greater than the proportion of the rest of the globe? Here, we'll 
# consider the other continents as one group and conduct a two-sample test. 

cat_test_data <- project_data %>% drop_na() %>%
  mutate(is_greater_60 = 
           case_when(
             life_exp > 60 ~ 1,
             life_exp <= 60 ~ 0
           ),
         samp =
           case_when(
             continent == "South America" ~ "South America",
             TRUE ~ "Global"
           )
  ) %>% 
  select(samp, is_greater_60) %>% 
  group_by(samp) %>% 
  summarize(count = n(),
            prop_greater_60 = sum(is_greater_60)/count)

s_amer_prop <- cat_test_data %>% 
  filter(samp == "South America")

global_prop <- cat_test_data %>% 
  filter(samp != "South America")

test_stat_2 <- categorical_z(
  p_hat = s_amer_prop$prop_greater_60,
  p = global_prop$prop_greater_60,
  n = s_amer_prop$count
)

pnorm(test_stat_2, lower.tail = FALSE)  

# Since the p-value is nearly 0, we reject the null hypothesis. The proportion
# of South American countries with a life expectancy above 60 is significantly 
# different from the rest of the globe.

