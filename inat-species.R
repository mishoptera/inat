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
bounds <- c(25, -125.1, 49.5, -66.7)

# retrieve observations
sp_all <- get_inat_obs(taxon_name = "Capsella bursa-pastoris", 
                       quality = "research", 
                       maxresults = 99999,
                       bounds = bounds)

# map observations
sp_map <- inat_map(sp_all, plot = FALSE)
sp_map + borders("state") + theme_bw()

# *************************************************************
# LINK WITH NLCD DATA
coords <- sp %>% select(longitude, latitude) %>%
  na.omit()
sp_points <- SpatialPoints(coords, proj4string=CRS("+proj=longlat +datum=WGS84"))
usa_nlcd <- get_nlcd(template = sp_points, label = "USA")

# or try just downloading the file and working with it through something
# like this: https://rpubs.com/msundar/large_data_analysis

