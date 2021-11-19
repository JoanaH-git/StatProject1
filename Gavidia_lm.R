#Stuart Gavidia
#Project Part III

#Load packages
library(stat242)
library(dplyr)
library(tidyr)
library(ggthemes)
library(ggplot2)
library(moderndive)
#Load files from Part II
source(here::here("gavidia_dataPart2.R"))

#Checking correlation
get_correlation(data = project_data %>% group_by(continent), 
                formula = year ~ population, na.rm = TRUE)

#Checking individual continent (Going to focus on North America)
NorthAmerica <- project_data %>% 
  filter(continent == "North America")

year_population <- lm(formula = year ~ population, 
                   data = NorthAmerica)

#Regression Table
reg_table <- tibble(get_regression_table(year_population))

#Plotting Regression Table
ggplot(NorthAmerica, aes(year, population, size = population/10^6))+
  geom_point(alpha = 0.2)+
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE)+
  labs(
    title = "Year v Population",
    subtitle = "1980-2018",
    size = "Population (millions)",
    x = "Year",
    y = "Population",
    caption = "Source: Stat242"
  )+
  theme_classic()+ # from ggthemes
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

#Check for Outliers
ggplot(NorthAmerica, aes(year, population, size = population/10^6))+
  geom_point(alpha = 0.2)+
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE)+
  #facet_wrap(~year)+
  labs(
    title = "Year v Population",
    subtitle = "1980 - 2018",
    size = "Population (millions)",
    x = "Year",
    y = "Population",
    caption = "Source: Stat242"
  )+
  theme_grey()+ # from ggthemes
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

#Outliers not found
#No need to filter
#Use original Linear Model

