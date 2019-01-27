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
# To make it easier, limit it to just the biggest cities in the country
library(htmltab)
table <- htmltab("https://en.wikipedia.org/wiki/List_of_United_States_cities_by_population",5)

# ******
# LINK WITH NLCD DATA
coords <- sp_all %>% select(longitude, latitude) %>%
  na.omit()
sp_points <- SpatialPoints(coords, proj4string=CRS("+proj=longlat +datum=WGS84"))
usa_nlcd <- get_nlcd(template = sp_points, label = "USA")
# above doesn't really make sense. I'm trying to cover too large of an area.
# or try just downloading the file and working with it through something
# like this: https://rpubs.com/msundar/large_data_analysis
# seems like it might be best to run this in python which is a good excuse
# for me to finally learn how to do this.  installing reticulate package so
# I can connect the two as necessary.

# new idea is to export to a google fusion table and run some script in the google
# earth engine.

city_points <- na.omit(data.frame(run_cities))
coordinates(city_points) = ~longitude + latitude
proj4string(city_points) <- CRS("+init=epsg:4326")
city_points2 <- spTransform(city_points, proj4string(run_rasters))
city_points3 <- raster::extract(run_rasters, city_points, sp=TRUE)
city_points4 <- as.data.frame(spTransform(city_points3, CRS("+init=epsg:4326")))
names(city_points4)[39] <- "NLCD_layer" #for 2016 it's 39, for 2017 it's 38, for 2018 its 37



