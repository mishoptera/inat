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
load('data/nlcd_codes.Rdata')

# source files
source("functions/functions.R")
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
# To make it easier to process, limited to just the biggest cities in the country which
# were pulled from US census as cities over 100,000 residents.

# to test functions
city_name <- i <- "Oakland, CA"

# create a template to be added to. Don't forget to delete later
template <- sp_all %>%
  slice(1) %>%
  mutate(nlcd_code = NA) %>%
  select(-c(latitude, longitude), everything()) %>%
  mutate(nlcd_simple = NA, nlcd_category = NA, nlcd_desc = NA, city = NA)

# decide which cities to run code on (these are the cities that are estimated to have over 10 obs
initial_run <- city_priority(sp_all, cities)

# call function and add all together
sp_all_wNLCD <- template %>%
  bind_rows(lapply(initial_run, function(i){ city_nlcd(i, sp_all, template)})) %>%
  slice(-1) %>%
  distinct(id, .keep_all = TRUE)








