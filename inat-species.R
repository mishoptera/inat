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

# decide which cities to run code on (these are the cities that are estimated to have over 10 obs
initial_run <- city_priority(sp_all, cities)

# create a template to be added to. Don't forget to delete later
template <- sp_all %>%
  slice(1) %>%
  mutate(nlcd_code = NA) %>%
  select(-c(latitude, longitude), everything()) %>%
  mutate(nlcd_simple = NA, nlcd_category = NA, nlcd_desc = NA, city = NA)


# call function and add all together
lapply (initial_run, function(i){ 
  print(i)
  assign("template", city_nlcd(i, sp_all, template), envir = .GlobalEnv)
  })

sp_all_wNLCD <- template %>%
  slice(-1) %>%
  distinct(id, .keep_all = TRUE)


# *************************************************************
# SOME EXPLORATION OF THIS...
# *************************************************************
# map observations
sp_map_wNLCD <- inat_map(sp_all_wNLCD, plot = FALSE)
sp_map_wNLCD + borders("state") + theme_bw()


# how many observations are left after filtering by cities?
sp_all_wlNLCD %>% distinct (id)


# summary table of number of obs in each land cover type by city
nlcd_summary <- sp_all_wNLCD %>%
  group_by(city, nlcd_simple) %>%
  summarise(count = n()) %>%
  spread(nlcd_simple, count) %>%
  select (city, n, d1, d2, d3, d4, everything())





