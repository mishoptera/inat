# Author: Misha Leong
# Date: February 2019
# Project: Using iNat and NLCD data to look for patterns
# Specifically: Functions used in main script


# *************************************************************
# FIRST THINGS FIRST
# *************************************************************
# load libraries
library(tidyverse)


# *************************************************************
# FUNCTION TO PRIORITIZE WHICH CITIES TO FOCUS ON
# *************************************************************
# estimate iNat observations with place_guess for observations that match basic city string.
# to use in conjunction with place_guess (not perfect because sometimes place_guess write out
# full name of state, or otherwise messes things up for us)
city_priority <- function(sp_all, cities){
  cities_match <- str_c(cities, collapse = "|")
  
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
  
  return (cities_to_nlcd)
  
}

# *************************************************************
# FUNCTION TO MATCH NLCD VALUES FOR EACH OVSERVATION
# *************************************************************
# get NLCD tile for city
city_nlcd <- function (city_name, sp_all, template) {
  city <- get_map(city_name, zoom = 10)
  bb<-attr(city, 'bb')
  extentB <- polygon_from_extent(raster::extent(bb$ll.lon, bb$ur.lon, bb$ll.lat, bb$ur.lat), proj4string = "+proj=longlat +ellps=GRS80   +datum=NAD83 +no_defs")
  city_nlcd <- get_nlcd (template = (extentB), label = city_name)
  # next subset all observations by the same bounding box and then perform the match.
  sp_city <- sp_all %>%
    filter(latitude > bb$ll.lat & latitude < bb$ur.lat &
             longitude > bb$ll.lon & longitude < bb$ur.lon)
  # make inat data spatial
  sp_points <- data.frame(sp_city)
  coordinates(sp_points) = ~longitude + latitude
  # transform sp_poiints data to the appropriate NLCD tile
  proj4string(sp_points) <- CRS("+init=epsg:4326")
  sp_points2 <- spTransform(sp_points, proj4string(city_nlcd))
  #extract value to points
  sp_points3 <- raster::extract(city_nlcd, sp_points2, sp=TRUE)
  #make it back into a normal dataframe again
  sp_city_wNLCD <- spTransform(sp_points3, CRS("+init=epsg:4326")) %>%
    as.tibble() %>%
    rename("nlcd_code" = !!names(.[36])) %>%
    left_join(nlcd_codes, by = "nlcd_code") %>%
    # add city name tag (downside is for cities that overlap like Oakland, CA and we need
    # to delete duplicates. some San Jose sites might read as San Francisco, CA. not too bad?)
    mutate (city = city_name) %>%
    bind_rows(template, .)
  return(sp_city_wNLCD)
}

# *************************************************************
# FUNCTION TO DETERMINE SLOPE FOR CITY
# *************************************************************
get_slope <- function(n, d1, d2, d3, d4) { 
  x <- c(1:5)
  y <- c(n, d1, d2, d3, d4)
  slope_result <- lm(y~x, na.action=na.exclude)$coeff[[2]]
  return(slope_result)
}