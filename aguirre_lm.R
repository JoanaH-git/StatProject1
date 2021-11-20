# Load packages --------------------------------------------------------------

library(ggplot2)
library(ggthemes)
library(dplyr)
library(moderndive)
library(stat242)

# Load data and plot from part 2 ---------------------------------------------
source(here::here("aguirre_data-viz.R"))
project_data <- stat242::project_data

# Check correlation ----------------------------------------------------------


get_correlation(data = project_data %>% group_by(continent), 
                formula = life_exp[!is.na(life_exp)] ~ year)


# Build model ----------------------------------------------------------------
# I'll focus on Europe (you can focus on one continent/country as well) since
# it had a high correlation value and relatively large sample size 

s_amer <- project_data %>% 
  filter(continent == "South America")

life_year_mod <- lm(formula = life_exp ~ year, 
                   data = s_amer)

# Get the regression table
reg_table <- tibble(get_regression_table(life_year_mod))

# Plot regression model
ggplot(s_amer, aes(year, life_exp, size = population/10^6))+
  geom_point(alpha = 0.2)+
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE)+
  labs(
    title = "Life Expectancy v. Net Users in South America",
    subtitle = "1979 - 2018",
    size = "Population (millions)",
    x = "Year",
    y = "Life Expectancy",
    caption = "Source: Stat242"
  )+
  theme_economist()+ # from ggthemes
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

