#Stuart Gavidia
#Continents, num_phones, and population

library(stat242)
library(dplyr) # For data wrangling
library(tidyr) # For data tidying


View(project_data)
#test comment
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








