#Group Submission Part II
#Lisa, Eric, Joana, Stuart

library(dplyr) # For data wrangling
library(tidyr) # For data tidying
library(ggthemes) # For presentation plots
library(ggplot2) # For plotting data
library(stat242) # For data
library(patchwork) # For aesthetics

#Group scatterplot
# Life Expectancy v net users
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

# A Scatterplot of num_phones v year
ggplot(project_data, aes(year, num_phones))+
  geom_point()

## Extra Scatterplot Net_users v Year
ggplot(project_data, aes(year, net_users))+
  geom_point()

