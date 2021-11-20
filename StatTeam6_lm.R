# Load packages --------------------------------------------------------------

library(ggplot2)
library(ggthemes)
library(dplyr)
library(moderndive)
library(stat242)

# Load data and plot from part 2 ---------------------------------------------
source(here::here("StatTeam6_data-viz.R"))
project_data <- stat242::project_data

# Check correlation ----------------------------------------------------------


get_correlation(data = project_data %>% group_by(continent), 
                formula = life_exp[!is.na(life_exp)] ~ net_users[!is.na(net_users)])


# Build model ----------------------------------------------------------------

life_net_mod <- lm(formula = life_exp ~ net_users, 
                   data = project_data)

# Get the regression table
reg_table <- tibble(get_regression_table(life_net_mod))

# Plot regression model
ggplot(project_data, aes(net_users, life_exp, size = population/10^6))+
  geom_point(alpha = 0.2)+
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE)+
  labs(
    title = "Life Expectancy v. Net Users Globally",
    subtitle = "1979 - 2018",
    size = "Population (millions)",
    x = "Net Users",
    y = "Life Expectancy",
    caption = "Source: Stat242"
  )+
  theme_economist()+ # from ggthemes
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# Since the size of the points is the population, its reasonable to 
# guess that the aparent outliers are the same country. Let's check
# with a facet wrap by year.
ggplot(project_data, aes(net_users, life_exp, size = population/10^6))+
  geom_point(alpha = 0.2)+
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE)+
  facet_wrap(~year)+
  labs(
    title = "Life Expectancy v. Net Users Globally",
    subtitle = "1979 - 2018",
    size = "Population (millions)",
    x = "Net Users",
    y = "Life Expectancy",
    caption = "Source: Stat242"
  )+
  theme_economist()+ # from ggthemes
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

# This suggests it is one country. Let's remove it and rerun the analysis 
# to see if it improves. 

# It's China and India! 

project_data_2 <- project_data %>% 
  filter(country != "China")

project_data_2 <- project_data_2 %>% 
  filter(country != "India")

get_correlation(project_data_2, life_exp ~ net_users)

project_data_2_model <- lm(life_exp ~ net_users, data = project_data_2)

project_data_2_model_summary <- tibble(get_regression_table(project_data_2_model))

ggplot(project_data_2, aes(net_users, life_exp, size = population/10^6))+
  geom_point(alpha = 0.2)+
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE)+
  labs(
    title = "Life Expectancy v. Net Users Globally",
    subtitle = "1979 - 2018",
    size = "Population (millions)",
    x = "Net Users",
    y = "Life Expectancy",
    caption = "Source: Stat242"
  )+
  theme_economist()+ # from ggthemes
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))
