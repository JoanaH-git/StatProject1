#Stuart Gavidia
#Project Part II
#Continents, num_phones, and population

library(stat242)
library(dplyr)
library(tidyr)
library(ggthemes)
library(ggplot2)

#Create Scatterplots, boxplots, and or histograms
#Histogram for population by continent
ggplot(project_data, aes(population, fill = as.factor(year))) +
  geom_histogram()+
  facet_wrap(~continent)

#Scatterplot for year versus population
## year v population
ggplot(project_data, aes(year, population))+
  geom_point()

## year v population separated by continent
ggplot(project_data, aes(year, population, color = continent))+
  geom_point()+
  facet_wrap(~continent)

#Box plot
ggplot(project_data, aes(population, reorder(continent, population)))+
  geom_boxplot(outlier.alpha = 0.5)

