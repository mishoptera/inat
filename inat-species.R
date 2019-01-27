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

# javascript code to try
// As I can't access your FusionTable,
// I make random points and create a FeatureCollection
var p1 = ee.Geometry.Point([142.36083984375, -37.466138602344046])
var p2 = ee.Geometry.Point([143.23974609375, -37.04640889969956])
var pts = ee.FeatureCollection(ee.List([ee.Feature(p1),ee.Feature(p2)]))

//IMPORT LANDSAT IMAGE
var L82014pre = ee.ImageCollection('LANDSAT/LC8_SR') //Landsat 8 Surface   reflectance
.filter(ee.Filter.eq('wrs_path', 94))
.filter(ee.Filter.eq('wrs_row', 86)) 
.filterDate(ee.Date.fromYMD(2013,12,13), ee.Date.fromYMD(2014,1,15))

// Empty Collection to fill
var ft = ee.FeatureCollection(ee.List([]))

var fill = function(img, ini) {
// type cast
var inift = ee.FeatureCollection(ini)

// gets the values for the points in the current img
var ft2 = img.reduceRegions(pts, ee.Reducer.first(),30)

// gets the date of the img
  var date = img.date().format()

// writes the date in each feature
var ft3 = ft2.map(function(f){return f.set("date", date)})

// merges the FeatureCollections
return inift.merge(ft3)
}

// Iterates over the ImageCollection
var newft = ee.FeatureCollection(L82014pre.iterate(fill, ft))

// Export
Export.table.toDrive(newft,
"anyDescription",
"anyFolder",
"anyNameYouWant")

