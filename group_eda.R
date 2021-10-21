#Group Submission Part I
#Lisa, Eric, Joana, Stuart

library(stat242)
library(dplyr)
library(tidyr)

#Eric code
#Five number summary grouped by continents for life expectancy
life_exp_summary <- project_data %>% 
  group_by(continent) %>% 
  summarize(five_num = fivenum(life_exp),
            num_countries = n()/12) %>% 
  mutate(name = c("min", "q1", "med", "q3", "max")) %>% 
  pivot_wider(names_from = name, 
              values_from = five_num)

#Five number summary across entire globe for life expectancy
global_life_exp_summmary <- project_data %>% 
  summarize(five_num = fivenum(life_exp))


#Mean and sd for life expectancy
life_stats_by_country <- project_data %>% 
  group_by(country) %>% 
  summarize(life_mean = mean(life_exp),
            life_sd = sd(life_exp))

life_stats_by_year <- project_data %>% 
  group_by(year) %>% 
  summarize(life_mean = mean(life_exp, na.rm=TRUE),
            life_sd = sd(life_exp, na.rm=TRUE))

#Lisa code
#Five number summary by continent for net users
gdp_summary <- project_data %>% 
  group_by(continent) %>% 
  summarize(five_num = fivenum(net_users),
            num_countries = n()/12) %>% 
  mutate(name = c("min", "q1", "med", "q3", "max")) %>% 
  pivot_wider(names_from = name, 
              values_from = five_num)

#Five number summary for net users globally across all years
global_gdp_summmary <- project_data %>% 
  summarize(five_num = fivenum(net_users))

#Mean and standard deviation for Net Users
life_stats <- project_data %>% 
  group_by(country) %>% 
  summarize(net_users_mean = mean(net_users),
            net_users_sd = sd(net_users))
#Mean and standard deviation for Non Net Users
pop_stats <- project_data %>% 
  group_by(country) %>% 
  summarize(non_net_users_mean = mean(non_net_users),
            non_net_users_sd = sd(non_net_users))

#Joana code
#Mean and standard deviation for num_phones by country 
num_phones <- project_data %>% 
  group_by(country) %>% 
  summarize(num_phones_mean = mean(num_phones, na.rm = TRUE),
            num_phones_sd = sd(num_phones, na.rm = TRUE))

#Five number summary for num_phones for each country across all years
num_phones_5 <- project_data %>% 
  group_by(country) %>% 
  summarize(five_num = fivenum(num_phones)) %>% 
  mutate(name = c("min", "q1", "med", "q3", "max")) %>% 
  pivot_wider(names_from = name, 
              values_from = five_num)

#Stuart Code
#Means within continents for num_phones and population
continentMeans <- project_data %>% 
  group_by(continent) %>%
  summarize(cellPhonesMean = mean(num_phones, na.rm = TRUE),
            populationMean = mean(population, na.rm = TRUE))

#Five number summaries grouped by continents for num_phones and population
continentFive <- project_data %>% 
  group_by(continent) %>% 
  summarize(phoneFive = fivenum(num_phones),
            num_countries = n()/12,
            populationFive = fivenum(population),
            num_countries = n()/12) %>% 
  mutate(name = c("min", "q1", "med", "q3", "max")) %>% 
  pivot_wider(names_from = name,
              values_from = c(phoneFive, populationFive))