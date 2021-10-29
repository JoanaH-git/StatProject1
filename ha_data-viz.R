# Load Packages -----------------------------------------------------------

library(dplyr) # For data wrangling
library(tidyr) # For data tidying
library(ggthemes) # For presentation plots
library(ggplot2) # For plotting data
library(gapminder) # For data

# Load data from part I ---------------------------------------------------

source(here::here("stat242/ha_eda.R"))

# Histograms --------------------------------------------------------------

## Histogram for net_users with no grouping
ggplot(project_data, aes(net_users)) +
  geom_histogram()

## Grouping by continent via color
ggplot(project_data, aes(net_users, fill = continent)) +
  geom_histogram(position = "dodge")

# Scatter plots -----------------------------------------------------------

## Net_users v Year
ggplot(project_data, aes(year, net_users))+
  geom_point()

## Net_users v Life Expectancy
ggplot(project_data, aes(life_exp, net_users))+
  geom_point()

## Net_users v Population
ggplot(project_data, aes(population, net_users))+
  geom_point()

## Net_users v Num_phones
ggplot(project_data, aes(num_phones, net_users))+
  geom_point()

## Net_users v Non_net_users
ggplot(project_data, aes(non_net_users, net_users))+
  geom_point()

# Box plot ----------------------------------------------------------------

ggplot(project_data, aes(net_users, reorder(continent, net_users)))+
  geom_boxplot(outlier.alpha = 0.5)
