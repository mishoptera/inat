# Author: Misha Leong
# Date: January 2019
# Project: Using iNat and NLCD data to look for patterns


# *************************************************************
# FIRST THINGS FIRST
# *************************************************************
# load libraries
library(tidyverse)
library(ggmap)
library(rinat)
library(FedData)
library(reticulate)

# load files
load('data/all_inat.Rdata')
load('data/cities.Rdata')


# *************************************************************
# City Nature Challenge Data
# *************************************************************
# Run using the example of shepherd's purse
sp_cnc <- all_inat %>%
  filter(scientific_name == "Capsella bursa-pastoris") %>%
  group_by(hometown) %>%
  summarise(count = n())
sp_cnc


# *************************************************************
# ALL RESEARCH GRADE OBS
# *************************************************************
# To make it easier to process, limit it to just the biggest cities in the country
# This script creates a list of the largest cities in the country to compare against the iNat list
library(htmltab)
# downloading table of the largest cities in the US
bigCities <- htmltab("https://en.wikipedia.org/wiki/List_of_United_States_cities_by_population",5) 
bigCities2 <- bigCities[2:3] %>%
  as.tibble()

# downloading a table of cities and their abbreviations to cross reference
states <- htmltab("https://en.wikipedia.org/wiki/List_of_states_and_territories_of_the_United_States", 1)
colnames(states)[1:2] <- c("State", "usps")
states2 <- states %>% 
  select(State:usps) %>%
  as.tibble() 

# combining the above to create a list of common cities to cross-ref with iNaturalist place_guess
cities <- bigCities2 %>%
  left_join(states2, by = "State") %>%
  unite(cityNames, City, usps, sep = ", ")

# seartch iNat observations place_guess for observations that match this string!



# set up bounding box for US only (optional)
bounds <- c(25, -125.1, 49.5, -66.7) #all US


# retrieve observations
sp_all <- get_inat_obs(taxon_name = "Capsella bursa-pastoris", 
                       quality = "research", 
                       maxresults = 99999,
                       bounds = bounds)

# map observations
sp_map <- inat_map(sp_all, plot = FALSE)
sp_map + borders("state") + theme_bw()

# *************************************************************

# ************************************

coords <- sp_all %>% select(longitude, latitude) %>%
  na.omit()
sp_points <- SpatialPoints(coords, proj4string=CRS("+proj=longlat +datum=WGS84"))
usa_nlcd <- get_nlcd(template = sp_points, label = "USA")


# LINK WITH NLCD DATA
# 

city_points <- na.omit(data.frame(run_cities))
coordinates(city_points) = ~longitude + latitude
proj4string(city_points) <- CRS("+init=epsg:4326")
city_points2 <- spTransform(city_points, proj4string(run_rasters))
city_points3 <- raster::extract(run_rasters, city_points, sp=TRUE)
city_points4 <- as.data.frame(spTransform(city_points3, CRS("+init=epsg:4326")))
names(city_points4)[39] <- "NLCD_layer" #for 2016 it's 39, for 2017 it's 38, for 2018 its 37



