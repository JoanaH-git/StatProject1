# Project 1 Part 2
# Author: Joana Hadzhilazova
# Date: 10/28/21
# Variables used: num_phones, continent, year, non_net_users

library(stat242)
library(dplyr) # For data wrangling
library(tidyr) # For data tidying
library(ggplot2) #For graphing
View(project_data)
View(num_phones)
View(num_phones_5)
View(continent)
View(year)
View(non_net_users)


## Make Histograms

# Histogram without grouping
ggplot(data = project_data, mapping = aes(x = num_phones)) +
  geom_histogram()

# Historgram with grouping; num_phones v continent
ggplot(project_data, aes(num_phones, fill = continent)) +
  geom_histogram(position = "dodge")

## Make A Scatterplot

# A Scatterplot of num_phones v year
ggplot(project_data, aes(year, num_phones))+
  geom_point()

## Make a Boxplot

# Make a boxpot of continent v non_net_users
ggplot(project_data, aes(non_net_users, reorder(continent, non_net_users)))+
  geom_boxplot(outlier.alpha = 0.5)


