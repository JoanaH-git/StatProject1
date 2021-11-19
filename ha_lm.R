# Load packages --------------------------------------------------------------

library(ggplot2)
library(dplyr)
library(moderndive)

# Load data and plot from part 2 ---------------------------------------------
source(here::here("ha_data-viz.R"))

# Check correlation ----------------------------------------------------------

get_correlation(data = project_data %>% group_by(continent), 
                formula = life_exp ~ net_users)

# Given the sample sizes (count from the continents data) all of these are 
# actually enough to say there is a correlation between gdp per capita and 
# life expectancy. For the continents where it is lower, there more significant
# contributing factors.

# Build model ----------------------------------------------------------------
# I'll focus on Europe (you can focus on one continent/country as well) since
# it had a high correlation value and relatively large sample size 

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

# This suggests it is one country. Let's remove it and rerun the analysis 
# to see if it improves. 

asia %>% filter(life_exp < 50)

# China

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

