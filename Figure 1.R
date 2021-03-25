rm(list=ls())
# Load packages
library(rgdal)
library(gdalUtils)
library(raster)
library(ggpubr)
library(dplyr)
library(sp)
library(ggrepel)

# Coordinates for major cities
lon = c(-121.893028,-122.431297,-121.478851,-122.271111,-119.772591,-121.275604,-120.994446,-122.080795,-121.988571,-122.720306,-122.036346,-122.531087,-121.655502,-119.300003,-121.837479)
lat = c(37.335480,37.773972,38.575764,37.804363,36.746841,37.961632,37.661388,37.668819,37.548271,38.444660,37.368832,37.973535,36.677738,36.316666,39.728493)
name = c("San Jose","San Francisco","Sacramento","Oakland","Fresno","Stockton","Modesto","Hayward","Fremont","Santa Rosa","Sunnyvale","San Rafael","Salinas","Visalia","Chico")
cities = cbind.data.frame(lon,lat,name)

# Coordinates for major wildfires
lon = c(-121.30435,-122.244556)
lat = c(37.439437,37.184111)
name = c("SCU","CZU")
wildfires = cbind.data.frame(lon,lat,name)

# Read in MODIS data
setwd("C:/Users/Peng Shen/Dropbox/Shen_et_al_2021/R code/raster")
list.filenames = list.files()
list.filenames
# The 1st file covers Pacific ocean, only used for plotting purpose.
# The 2nd to 12th files are MODIS data covering California region, each recording eight days, from July 11th to October 6th.
files= list()
for (i in 1:12){
  r = raster(readGDAL(get_subdatasets(list.filenames[i])[1]))
  temp = data.frame(coordinates(r)[,1],coordinates(r)[,2],as.data.frame(r$band1))
  colnames(temp) = c("X","Y","band1")
  # Transform to Longitude and Latitude coordinates
  SP = cbind(as.data.frame(spTransform(SpatialPoints(temp[,1:2], proj4string=CRS(as.character(crs(r)))), CRS("+proj=longlat +datum=WGS84"))),temp$band1)
  colnames(SP) = c("lon","lat","band")
  SP = filter(SP,lat >= 35)
  files[[i]] = SP
}

ocean = files[[1]]
ocean = filter(ocean,lon <= -122,lon >= -124,lat <= 36.25)

# Select data for before wildfires
before = files[[2]]
for (i in 3:6){
  before = cbind(before,files[[i]][,3])
}
before$max = apply(before[,3:dim(before)[2]],1,max)
before = before[,c(1:2,dim(before)[2])]
before = filter(before,lon <= -117.5,lon >= -124, max >=3, max !=6)
before$max[which(before$max==4)] = 3
before$max[which(before$max==7 | before$max==8)] = 9

# Select data for during wildfires
during = files[[7]]
for (i in 8:12){
  during = cbind(during,files[[i]][,3])
}
# Find and select the max value for each file
during$max = apply(during[,3:dim(during)[2]],1,max)
during = during[,c(1:2,dim(during)[2])]
# Exclude not processed data pixels
during = filter(during,lon <= -117.5,lon >= -124, max >=3, max !=6)
# Treat cloud as water
during$max[which(during$max==4)] = 3
# Assign fire of different confidence levels with a same value
during$max[which(during$max==7 | during$max==8)] = 9

# Figure 1
ggplot()+
  geom_point(data=during,aes(lon,lat,color=as.factor(max)))+
  scale_color_manual(values = c("3"="blue","5" = "white","9" = "red"),labels = c("Water", "Land", "Fire"))+
  coord_fixed() + theme_grey(base_size = 20) + theme(legend.title = element_blank()) +
  geom_point(data=cities,aes(lon,lat),size = 4,color="yellow") +geom_point(data=wildfires,aes(lon,lat),size = 4,color="red")+
  geom_text_repel(cities,mapping = aes(lon,lat,fontface=2,label=name),size = 5,box.padding = unit(0.35, "lines"),point.padding = unit(0.3, "lines"))+
  geom_text(wildfires,mapping = aes(lon,lat,fontface=2,label=name),color="red",nudge_y = 0.2,size = 5)+
  geom_point(data=ocean,aes(lon,lat),color="blue") 

# Calculate total area of fire
sumfire = as.data.frame(spTransform(SpatialPoints(during, proj4string=CRS("+proj=longlat +datum=WGS84")),CRS(as.character(crs(r)))))
# Each pixel is 1km^2
sum(sumfire$max==9)
