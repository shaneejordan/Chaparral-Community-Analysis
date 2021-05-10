#Chaparral map

#clear the environment
rm(list=ls())

# Load libraries
library(tidyverse)
library(here)
library(maps)
library(mapdata)
library(mapproj)
library(dplyr)
library(ggplot2)
library(ggpubr)
library(raster)
library(scales)
library(rgeos)

#disjunct chaparral density
df<-read.csv('Chaparral_Density_Maps/Data/ChaparralDensity.csv')
density<-df %>%
  rename(lat = ï..lat)
view(density)

# get data for counties
counties<-map_data("county")
counties<-as.data.frame(counties)
LASB<-counties %>%
  filter(subregion == c("los angeles", "san bernardino"))
  counties[counties$subregion == "los angeles", "subregion"] <- "Los Angeles"
view(LASB)

legend_title <- "OMG My Title"

#make CA map with chaparral density embedded in
p1<-ggplot()+
  geom_polygon(data = LASB, 
               aes(x = long, 
                   y = lat,
                   group = group,
                  fill = subregion))+
  geom_point(shape=21,data = density, 
             aes(x = long, 
                 y = lat,
                 size = density))+
  #geom_text(data = density, aes(x=long, y=lat, label=density))+
  theme(panel.background = element_rect(fill = "white"))+
  scale_fill_discrete(labels = c("Los Angeles", "San Bernardino"))+
  coord_map(projection = "sinusoidal")
  ggsave(here("Final_Project","output","ChaparralDensity.png"))

  #New York Mnts
  #make zoomed in graphs w/ raster layer
  #get elevation data
  dem1<- getData("SRTM",lat=35.2,lon=-115.35)
  dem2<- getData("SRTM",lat=35.3,lon=-115.25)
  dem <- merge(dem1,dem2)
  
  #create polygon to crop the elevation data file   
  Ps1 = as(extent(35.2, 35.3, -115.35, -115.25), 'SpatialPolygons')
  crs(Ps1) = "+proj=longlat +datum=WGS84 +no_defs"
  
  #lower the resolution to enable faster plotting
  dem_lower_res <- aggregate(dem, fact=10)
  dem.p  <-  rasterToPoints(dem_lower_res)
  df2 <-  data.frame(dem.p)
  colnames(df2) = c("lon", "lat", "alt")
  
  #make ggplot
  p2<-ggplot(df2) +
    geom_raster(aes(lon, lat, fill = alt))+
    scale_fill_gradientn(colours = terrain.colors(10))+
    geom_polygon(data= LASB, aes(x=long, y=lat), color= 'blue', fill= NA) +            
    coord_sf(xlim = c(-115.35, -115.25), ylim = c(35.2, 35.3))+
    geom_point(shape=21,data = density, 
               aes(x = long, 
                   y = lat,
                   size = density))+
    theme_bw()+
    theme(axis.text.x = element_text(size=6), axis.text.y = element_text(size=6))
  
  #San Bernardino Mnts
  #make zoomed in graphs w/ raster layer
  #get elevation data
  dem1<- getData("SRTM",lat=34.25,lon=-117.25)
  dem2<- getData("SRTM",lat=35.35,lon=-117.15)
  dem <- merge(dem1,dem2)
  
  #create polygon to crop the elevation data file   
  Ps1 = as(extent(34.25, 34.35, -117.25, -117.15), 'SpatialPolygons')
  crs(Ps1) = "+proj=longlat +datum=WGS84 +no_defs"
  
  #lower the resolution to enable faster plotting
  dem_lower_res <- aggregate(dem, fact=10)
  dem.p  <-  rasterToPoints(dem_lower_res)
  df2 <-  data.frame(dem.p)
  colnames(df2) = c("lon", "lat", "alt")
  
  #make ggplot  
  p3<-ggplot(df2) +
    geom_raster(aes(lon, lat, fill = alt))+
    scale_fill_gradientn(colours = terrain.colors(10))+
    geom_polygon(data= LASB, aes(x=long, y=lat), color= 'blue', fill= NA) +            
    coord_sf(xlim = c(-117.25, -117.15), ylim = c(34.25, 34.35))+
    geom_point(shape=21,data = density, 
               aes(x = long, 
                   y = lat,
                   size = density))+
    theme_bw()+
    theme(axis.text.x = element_text(size=6), axis.text.y = element_text(size=6))
  
  #San Gabriel Mnts
  #make zoomed in graphs w/ raster layer
  #get elevation data
  dem1<- getData("SRTM",lat=34.35,lon=-117.85)
  dem2<- getData("SRTM",lat=34.45,lon=-117.75)
  dem <- merge(dem1,dem2)
  
  #create polygon to crop the elevation data file   
  Ps1 = as(extent(34.35, 34.45, -117.85, -117.75), 'SpatialPolygons')
  crs(Ps1) = "+proj=longlat +datum=WGS84 +no_defs"
  
  #lower the resolution to enable faster plotting
  dem_lower_res <- aggregate(dem, fact=10)
  dem.p  <-  rasterToPoints(dem_lower_res)
  df2 <-  data.frame(dem.p)
  colnames(df2) = c("lon", "lat", "alt")
  
  #make ggplot
  p4<-ggplot(df2) +
    geom_raster(aes(lon, lat, fill = alt))+
    scale_fill_gradientn(colours = terrain.colors(10))+
    geom_polygon(data= LASB, aes(x=long, y=lat), color= 'blue', fill= NA) +            
    coord_sf(xlim = c(-117.85, -117.75), ylim = c(34.35, 34.45))+
    geom_point(shape=21,data = density, 
               aes(x = long, 
                   y = lat,
                   size = density))+
    theme_bw()+
    theme(axis.text.x = element_text(size=6), axis.text.y = element_text(size=6))
  
  #combine all 3 grids together into a single figure
  ggarrange(p4, p3, p2,
            ncol = 3, nrow = 1,
            common.legend = TRUE)
  