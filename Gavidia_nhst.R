#Stuart Gavidia
#Project Part IV
#Load Packages

library(openintro)
library(dplyr)
library(tidyr)
library(infer)
library(ggplot2)

#Load data form Project Part III
source(here::here("Gavidia_lm.R"))

#Viewing the data
View(project_data)

#Prediction: What would the population look like in North America in the year 2040?

NorthAmericaPred <- linear_model_prediction(year_population, 2040)

test_stat <- numeric_z(
  x = NorthAmericaPred,
  mu = mean(NorthAmerica$population, na.rm=TRUE),
  s = sd(NorthAmerica$population, na.rm=TRUE)
)

pnorm(test_stat, lower.tail = TRUE)

#Interpretation of data: The p-value was 0.365 which is greater than 0.05. So we fail to reject the Null Hypothesis.
## This means that the population in North America in the year 2040 will not be unusually low.



#For Group Project:
# Is the proportion in Mexico with internet users over 1000 in the year 2015 significantly different
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

#The p-value is greater than 0.05 so we fail to reject the null hypothesis.
##The proportion of net users in Mexico with net users above 1000 in the year 2015
##is significantly different from the rest of the globe.
