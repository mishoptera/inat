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
library(sf)

# load files
load('data/all_inat.Rdata')
load('data/cities.Rdata')
load('data/allCityNames.Rdata')

# source files
source("keys.R")
register_google(key = personal_google_api_key) 


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
# *************************************************************
# To make it easier to process, limited to just the biggest cities in the country

# to test functions
city_name <- i <- "Oakland, CA"

# get NLCD tile for city
city_nlcd <- function (city_name, sp_all) {
  city <- get_map(city_name, zoom = 9)
  bb<-attr(city, 'bb')
  extentB <- polygon_from_extent(raster::extent(bb$ll.lon, bb$ur.lon, bb$ll.lat, bb$ur.lat), proj4string = "+proj=longlat +ellps=GRS80   +datum=NAD83 +no_defs")
  city_nlcd <- get_nlcd (template = (extentB), label = city_name)
  # next subset all observations by the same bounding box and then perform the match.
  # how to do this subset? Sweet! do not need to do any spatial points magic. Just looks
  # at coordinates and see whether they fall between ll.lat and ur.lat and ll.lon and ur.lon!
  sp_city <- sp_all %>%
    filter(latitude > bb$ll.lat & latitude < bb$ur.lat &
             longitude > bb$ll.lon & longitude < bb$ur.lon)
  # great that works! new problem is for cities that overlap in bounding boxes how not to have 
  # duplicate processing?  I guess that's not such a big issue since it's just processing time,
  # and we can just use 'distinct()' later.
}




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



