# Load Packages----------------------------------------------------------

library(dplyr) # For data wrangling
library(tidyr) # For data tidying
library(stat242) # For data

# Load Data -------------------------------------------------------------

project_data <- stat242::project_data
View(project_data)

# Calculate summary statistics for categorical variables ----------------

## What countries and continents are represented?
countries <- project_data %>% 
  group_by(country) %>% 
  summarize(count = n())

## How many years do we have data for each country?
continents <- project_data %>% 
  group_by(continent) %>% 
  summarize(count = n())

# Descriptive statistics for numeric variables --------------------------

## What years do we have data on?
project_data %>% 
  summarize(unique(year))

## Do we have data for each country for each year?
unique(countries$count)

## Calculate the five number summary for net users for each continent across all years.
gdp_summary <- project_data %>% 
  group_by(continent) %>% 
  summarize(five_num = fivenum(net_users),
            num_countries = n()/12) %>% 
  mutate(name = c("min", "q1", "med", "q3", "max")) %>% 
  pivot_wider(names_from = name, 
              values_from = five_num)

## Calculate the five number summary for net users globally across all years
global_gdp_summmary <- project_data %>% 
  summarize(five_num = fivenum(net_users))

## Calculate the mean and standard deviation for net users by country
life_stats <- project_data %>% 
  group_by(country) %>% 
  summarize(net_users_mean = mean(net_users),
            net_users_sd = sd(net_users))

## Calculate the mean and standard deviation for non net users by country
pop_stats <- project_data %>% 
  group_by(country) %>% 
  summarize(non_net_users_mean = mean(non_net_users),
            non_net_users_sd = sd(non_net_users))

