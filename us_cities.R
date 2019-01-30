# Author: Misha Leong
# Date: January 2019
# Project: Using iNat and NLCD data to look for patterns
# Specifically: Scrape wikipedia for a list of big cities

# *************************************************************
# FIRST THINGS FIRST
# *************************************************************
# load libraries
library(tidyverse)
library(htmltab)


# *************************************************************
# WEBSCRAPING
# *************************************************************
# downloading table of the largest cities in the US
# 311 incorporated places in the United States with a population of at least 100,000 on July 1, 2017, 
# as estimated by the United States Census Bureau.
bigCities <- htmltab("https://en.wikipedia.org/wiki/List_of_United_States_cities_by_population",5)[2:3] %>%
  as.tibble()

# downloading a table of cities and their abbreviations to cross reference
states <- htmltab("https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States", 1)
colnames(states)[1:2] <- c("State", "usps")
states <- states %>% 
  select(State:usps) %>%
  as.tibble() 

# combining list of common cities with state abbreviations, this can be used
# to cross reference with place_guess or to pull nlcd tiles
allCityNames <- bigCities %>%
  left_join(states2, by = "State") %>%
  unite(cityNames, City, usps, sep = ", ") %>%
  pull(cityNames)
save(allCityNames, file = "data/allCityNames.RData")

# to use in conjunction with place_guess
cities_match <- str_c(cities, collapse = "|")

# *************************************************************
# COMPARE TO INATURALIST RECORDS
# *************************************************************
# set up bounding box for US only (optional)
bounds <- c(25, -125.1, 49.5, -66.7) #all US

# retrieve observations for a given species
sp_all <- get_inat_obs(taxon_name = "Capsella bursa-pastoris", 
                       quality = "research", 
                       maxresults = 99999,
                       bounds = bounds)

# seartch iNat observations place_guess for observations that match this string.
sp_cities <- sp_all %>%
  as.tibble() %>%  
  filter(str_detect(place_guess, cities_match)) %>%
  mutate (city = str_extract (place_guess, cities_match)) 

# how many cities have observations that match place_guess?, and which cities should we run NLCD on?
num_obs_per_city <- sp_cities %>%
  group_by (city) %>%
  summarise (obs = n()) %>%
  arrange (desc(obs)) 
num_obs_per_city

cities_to_nlcd <- num_obs_per_city %>% 
  filter(obs > 10) %>%
  pull(city)



