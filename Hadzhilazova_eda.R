# Project 1 Part 1
# Author: Joana Hadzhilazova
# Date: 10/20/21
# Variables used: num_phones and countries

            library(stat242)
            library(dplyr) # For data wrangling
            library(tidyr) # For data tidying
            View(project_data)
            View(num_phones)
            View(num_phones_5)

## Descriptive Statistics: Calculate the mean and standard deviation for num_phones by country 

num_phones <- project_data %>% 
  group_by(country) %>% 
  summarize(num_phones_mean = mean(num_phones, na.rm = TRUE),
            num_phones_sd = sd(num_phones, na.rm = TRUE))

## Calculate the five number summary for num_phones for each country across all years

num_phones_5 <- project_data %>% 
  group_by(country) %>% 
  summarize(five_num = fivenum(num_phones)) %>% 
  mutate(name = c("min", "q1", "med", "q3", "max")) %>% 
  pivot_wider(names_from = name, 
              values_from = five_num)
