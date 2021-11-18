options(scipen =999)
# Load Packages -----------------------------------------------------------

library(dplyr) # For data wrangling
library(tidyr) # For data tidying
library(ggthemes) # For presentation plots
library(ggplot2) # For plotting data
library(stat242) # For data
library(patchwork) # For aesthetics

# Load data from part I ---------------------------------------------------

source(here::here("Aguirre_eda.R"))
project_data <- stat242::project_data

# Histograms --------------------------------------------------------------

## Histogram for life expectancy with no grouping
ggplot(project_data, aes(life_exp)) +
  theme_clean()+
  geom_histogram(color="grey", fill="black")

## Grouping by continent via color
ggplot(project_data, aes(life_exp, fill = continent)) +
  theme_clean()+
  geom_histogram(position = "dodge", binwidth=1)

## Grouping by continent via facets
ggplot(project_data, aes(life_exp, fill = as.factor(year))) +
  geom_histogram()+
  facet_wrap(~continent)

# Scatter plots -----------------------------------------------------------

## Life Expectancy v net users
asia <- project_data %>%
  filter(continent=="Asia")
africa <- project_data %>%
  filter(continent=="Africa")
europe <- project_data %>%
  filter(continent=="Europe")
s_amer  <- project_data %>%
  filter(continent=="South America")
n_amer <- project_data %>%
  filter(continent=="North America")
oceania <- project_data %>%
  filter(continent=="Oceania")

asia_plot <- ggplot(asia, aes(net_users, life_exp))+
  geom_point()+
  labs(
    x = "Internet Users (millions)",
    y = "Life Expectancy"
  )+
  theme(axis.text.x = element_text(angle = 45))

africa_plot <- ggplot(africa, aes(net_users, life_exp))+
  geom_point()+
  labs(
    x = "Internet Users (millions)",
    y = "Life Expectancy"
  )+
  theme(axis.text.x = element_text(angle = 45))

euro_plot <- ggplot(europe, aes(net_users, life_exp))+
  geom_point()+
  labs(
    x = "Internet Users (millions)",
    y = "Life Expectancy"
  )+
  theme(axis.text.x = element_text(angle = 45))

s_am_plot <- ggplot(s_amer, aes(net_users, life_exp))+
  geom_point()+
  labs(
    x = "Internet Users (millions)",
    y = "Life Expectancy"
  )+
  theme(axis.text.x = element_text(angle = 45))

n_am_plot <- ggplot(n_amer, aes(net_users, life_exp))+
  geom_point()+
  labs(
    x = "Internet Users (millions)",
    y = "Life Expectancy"
  )+
  theme(axis.text.x = element_text(angle = 45))

ocea_plot <- ggplot(oceania, aes(net_users, life_exp))+
  geom_point()+
  labs(
    x = "Internet Users (millions)",
    y = "Life Expectancy"
  )+
  theme(axis.text.x = element_text(angle = 45))

asia_plot + africa_plot + euro_plot + s_am_plot + n_am_plot + ocea_plot+
  plot_annotation(title = "Life Expectancy v. Internet Users by Continent",
                  subtitle = "1979 - 2018",
                  caption = "Source: Gapminder")

## Life Expectancy v Year
ggplot(project_data, aes(year, life_exp, color=continent))+
  geom_point()

## Life Expectancy v Pop
ggplot(project_data, aes(population, life_exp, color=continent))+
  geom_point()+
  xlim(0, 350000000)

## Life Expectancy v number of phones
ggplot(project_data, aes(num_phones, life_exp, color = continent))+
  geom_point()+
  xlim(1,500000000)


# Box plot ----------------------------------------------------------------

ggplot(project_data, aes(life_exp, reorder(continent, life_exp), color=continent))+
  xlim(30, 90)+
  geom_boxplot(outlier.alpha = 0.5)
