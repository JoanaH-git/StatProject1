# Load Packages----------------------------------------------------------

library(dplyr) # For data wrangling
library(tidyr) # For data tidying
library(stat242) # For data

# Load Data -------------------------------------------------------------

project_data <- stat242::project_data
View(project_data)

# Calculate summary statistics for categorical variables ----------------

## What countries and continents are represented?
## How many years do we have data for each country?

countries <- project_data %>% 
  group_by(country) %>% 
  summarize(count = n())

continents <- project_data %>% 
  group_by(continent) %>% 
  summarize(count = n())

# Descriptive statistics for numeric variables --------------------------

## What years do we have data on?
project_data %>% 
  summarize(unique(year))

## Do we have data for each country for each year?
unique(countries$count)

## Calculate the five number summary for life expectancy for each continent across all years.
life_exp_summary <- project_data %>% 
  group_by(continent) %>% 
  summarize(five_num = fivenum(life_exp),
            num_countries = n()/12) %>% 
  mutate(name = c("min", "q1", "med", "q3", "max")) %>% 
  pivot_wider(names_from = name, 
              values_from = five_num)
## Calculate the five number summary for life expectancy globally across all years
global_life_exp_summmary <- project_data %>% 
  summarize(five_num = fivenum(life_exp))

## Calculate the mean and standard deviation for life expectancy by country
life_stats_by_country <- project_data %>% 
  group_by(country) %>% 
  summarize(life_mean = mean(life_exp),
            life_sd = sd(life_exp))

life_stats_by_year <- project_data %>% 
  group_by(year) %>% 
  summarize(life_mean = mean(life_exp, na.rm=TRUE),
            life_sd = sd(life_exp, na.rm=TRUE))
