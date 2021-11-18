# Title: "Project Part III: Building a Linear Model"
# Author: "Joana Hadzhilazova"
# Date: "11/18/21"

#Load Packages
library(ggplot2)
library(dplyr)
library(moderndive)
library(stat242)

## Load data and plot from Project Part II
source("Users/valeriailieva/Desktop/Fall 2021/Math 242/Hadzhilazova_data-viz.R")

## Checking the correlation
get_correlation(data = project_data %>% group_by(continent), 
                formula = year ~ num_phones,na.rm=TRUE)
#There is a correlation between num_phones and year throughout the continents. The most significant ones/strongest ones are Africa and and South America. This could be due to factors such as improvement in technology or surge in popularity of a certain phone company, new or affordable phones, etc.   

## Building a model
# For example focusing on Africa because it has the highest correlation
africa <- project_data %>% 
  filter(continent == "Africa" & num_phones > 0)

the_mod <- lm(formula = num_phones ~ year, 
                   data = africa)

#This is the model showing year v num_phones

# Get the regression table
reg_table <- tibble(get_regression_table(the_mod, digits = 8))

#Plotting the regression model

ggplot(africa, aes(year, num_phones, size = 10^7))+
  geom_point(alpha = 0.2)+
  geom_smooth(method = "lm", se = FALSE, show.legend = FALSE)+
  labs(
    title = "Year v. Number of Phones in Africa",
    subtitle = "1979 - 2018",
    size = "Quantity in Millions",
    x = "Year",
    y = "Number of Phones",
    caption = "Source: Stat242"
  )+
  theme_bw()+ # from ggplot2 themes
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1))

