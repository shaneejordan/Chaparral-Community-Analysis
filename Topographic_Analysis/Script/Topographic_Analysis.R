#Topographic analysis with preliminary data
#02/25/2021
#created by Shane E. Jordan

#Clear the environment
rm(list=ls())

# Load libraries
library(tidyverse)
library(here)
library(maps)
library(mapdata)
library(mapproj)
library(ggpubr)
library(raster)
library(scales)
library(rgeos)

#I'm going to generate slope, aspect, and TPI for one of the preliminary sites that I surveyed

#first, I will calculate coarse environmental data for the NY Mnts
#need to generate a digital elevation model (DEM)
DEM1 <- getData('SRTM',lat=35.2,lon=-115.35)
DEM2 <- getData('SRTM',lat=35.3,lon=-115.25)
NYMNTS_DEM <- merge(DEM1,DEM2)

#create a bounding box
lon <- c(-115.35, -115.25)
lat <- c(35.2, 35.3)
lon <- c(-115.35, -115.25)
lat <- c(35.2, 35.3)
bbox <- matrix(c(lon[1], lat[1], lon[2], lat[2]), 2, 2, 
               dimnames = list(c("lon", "lat"), c("min", "max")))

#extract the DEM using crop()
NYMNTS_DEM_crop <- crop(x = NYMNTS_DEM, y = bbox)
NYMNTS_DEM_crop

#calculate environmental data
env_data <- terrain(NYMNTS_DEM_crop, opt=c("slope", "aspect", "TPI"), unit="radians", neighbors=8, filename="")
env_data
plot(env_data)

env_data <- terrain(NYMNTS_DEM_crop, opt=c("TPI"), unit="radians", neighbors=8, filename="")
env_data
plot(env_data)



#second, calculate fine environmental data for one of the line transects
#need to generate a digital elevation model (DEM)
DEM3 <- getData('SRTM',lat=35.27,lon=-115.29)
DEM4 <- getData('SRTM',lat=35.26,lon=-115.30)
line_DEM <- merge(DEM3,DEM4)

#create a bounding box
lon <- c(-115.294, -115.293)
lat <- c(35.270, 35.269)
lon <- c(-115.294, -115.293)
lat <- c(35.270, 35.269)
bbox <- matrix(c(lon[1], lat[1], lon[2], lat[2]), 2, 2, 
               dimnames = list(c("lon", "lat"), c("min", "max")))

#extract the DEM using crop()
line_DEM_crop <- crop(x = line_DEM, y = bbox)
line_DEM_crop

#calculate environmental data
line_env_data<-terrain(line_DEM_crop, opt=c("slope", "aspect", "TPI"), unit="radians", neighbors=8, filename="")
line_env_data
plot(line_env_data)
