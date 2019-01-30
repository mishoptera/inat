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

# source files
source("keys.R")


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
# set up bounding box for US only (optional)
bounds <- c(25, -125.1, 49.5, -66.7) #all US

# retrieve observations for a given species
sp_all <- get_inat_obs(taxon_name = "Capsella bursa-pastoris", 
                       quality = "research", 
                       maxresults = 99999,
                       bounds = bounds)

# map observations
sp_map <- inat_map(sp_all, plot = FALSE)
sp_map + borders("state") + theme_bw()

# *************************************************************
# NLCD MATCHING
# To make it easier to process, limit it to just the biggest cities in the country
# This script creates a list of the largest cities in the country to compare against the iNat list

library(htmltab)
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
cities <- bigCities %>%
  left_join(states2, by = "State") %>%
  unite(cityNames, City, usps, sep = ", ") %>%
  pull(cityNames)

# to use in conjunction with place_guess
cities_match <- str_c(cities, collapse = "|")

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


# ************************************
# Actual matching to NLCD now.
cities_to_nlcd <- num_obs_per_city %>% 
  filter(obs > 10) %>%
  pull(city)

i <- "New York, NY"
city_name <- i
# r generic creating bounding box function
create_bb <- function (city_name) {
  city <- get_map(city_name, zoom = 9)
  bb<-attr(city, 'bb')
  extentB <- polygon_from_extent(raster::extent(bb$ll.lon, bb$ur.lon, bb$ll.lat, bb$ur.lat), proj4string = "+proj=longlat +ellps=GRS80   +datum=NAD83 +no_defs")
  return (extentB)
}

city_nlcd <- get_nlcd (template = (create_bb(i)), label = i)


process_cities <- function(city_name) {
  coords <- sp_all %>% select(longitude, latitude) %>%
    na.omit()
  sp_points <- SpatialPoints(coords, proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  
}

lapply(city_names, function(i){
  process_cities(sp_cities)
})

process_cities <- function(city_name) {
  coords <- sp_all %>% select(longitude, latitude) %>%
    na.omit()
  sp_points <- SpatialPoints(coords, proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  
}

# table that collapses all land cover types, but pulls out each city
big_simple_ranks <- simple_birds %>%
  bind_rows(simple_mammals, simple_reptiles, simple_amphibians, simple_gastropods, simple_insects, simple_dicots, simple_monocots, simple_ferns, simple_conifers) %>%
  left_join(names, by="scientific_name") %>%
  left_join(total_cities, by="scientific_name") %>%
  distinct(scientific_name, .keep_all = TRUE) %>%
  filter(num_cities>=4)
city_nlcd <- get_nlcd (template = (create_bb("Austin, TX")), label = 'Austin')



# LINK WITH NLCD DATA
# 

city_points <- na.omit(data.frame(run_cities))
coordinates(city_points) = ~longitude + latitude
proj4string(city_points) <- CRS("+init=epsg:4326")
city_points2 <- spTransform(city_points, proj4string(run_rasters))
city_points3 <- raster::extract(run_rasters, city_points, sp=TRUE)
city_points4 <- as.data.frame(spTransform(city_points3, CRS("+init=epsg:4326")))
names(city_points4)[39] <- "NLCD_layer" #for 2016 it's 39, for 2017 it's 38, for 2018 its 37



