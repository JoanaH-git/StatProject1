# Title: "Project Part III"
# Author: "Lisa Ha"
# Date: "11/18/21"

# Load packages --------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(moderndive)

# Load data and plot from part 2 ---------------------------------------------
source(here::here("ha_data-viz.R"))

# Check correlation ----------------------------------------------------------

get_correlation(data = project_data %>% group_by(continent), 
                formula = life_exp ~ net_users)

# Build model ----------------------------------------------------------------

asia <- project_data %>% 
  filter(continent == "Asia")

life_net_users_mod <- lm(formula = life_exp ~ net_users, 
                   data = asia)

# Get the regression table
reg_table <- tibble(get_regression_table(life_net_users_mod))

# Plot regression model
ggplot(asia, aes(net_users, life_exp, size = population/10^6))+
  geom_point(alpha = 0.2)+
  geom_smooth(method = "lm", col = "deepskyblue3", se = FALSE, show.legend = FALSE)+
  labs(
    title = "Life Expectancy v. Net Users in Asia",
    subtitle = "1979 - 2018",
    size = "Population (millions)",
    x = "Net Users",
    y = "Life Expectancy",
    caption = "Source: Gapminder"
  )+
  theme_solarized_2()+ # from ggthemes
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Since the size of the points is the population, its reasonable to 
# guess that the aparent outliers are the same country. Let's check
# with a facet wrap by year.
ggplot(asia, aes(net_users, life_exp, size = population/10^6))+
  geom_point(alpha = 0.2)+
  geom_smooth(method = "lm", col = "deepskyblue3", se = FALSE, show.legend = FALSE)+
  facet_wrap(~year)+
  labs(
    title = "Life Expectancy v. Net Users in Asia",
    subtitle = "1952 - 2007",
    size = "Population (millions)",
    x = "Net Users",
    y = "Life Expectancy",
    caption = "Source: Gapminder"
  )+
  theme_solarized_2()+ # from ggthemes
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Rerunning model - China

asia %>% filter(life_exp < 50)

asia_2 <- asia %>% 
  filter(country != "China")

get_correlation(asia_2, life_exp ~ net_users)

asia_2_model <- lm(life_exp ~ net_users, data = asia_2)

asia_2_model_summary <- tibble(get_regression_table(asia_2_model))

ggplot(asia_2, aes(net_users, life_exp, size = population/10^6))+
  geom_point(alpha = 0.2)+
  geom_smooth(method = "lm", col = "deepskyblue3", se = FALSE, show.legend = FALSE)+
  labs(
    title = "Life Expectancy v. Net Users in Asia",
    subtitle = "1952 - 2007",
    size = "Population (millions)",
    x = "Net Users",
    y = "Life Expectancy",
    caption = "Source: Gapminder"
  )+
  theme_solarized_2()+ # from ggthemes
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

