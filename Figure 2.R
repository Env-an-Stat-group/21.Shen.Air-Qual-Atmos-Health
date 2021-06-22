rm(list=ls())
# Load packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggmap)
library(stringr)
library(ggsn)
library(googleway)
register_google(key = "AIzaSyA3pSe3OIDFtQ2ga6N5lLVy6HinHXf7ZYo")

# Preprocessed PurpleAir date can be directly loaded
load("purple.RData")
# Read in PurpleAir data 
#setwd("~/Dropbox/Shen_et_al_2021/R code/Daily")
#list.filenames<-list.files()
#data = data.frame()
#for (i in 1:length(list.filenames)){
#  temp = read.csv(list.filenames[i],header=T)
#  if (dim(temp)[1] >= 5) {
#    temp = temp[,1:9]
#    starting = as.numeric(str_locate_all(pattern ='\\.',list.filenames[i])[[1]][1,2]) -2
#    temp$lat = as.numeric(substring(list.filenames[i],starting,starting+8))
#    temp$lon = as.numeric(substring(list.filenames[i],starting+9,starting+18))
#    temp$node = i
#    data = rbind.data.frame(data,temp)
#  }
#}
# Clean PurpleAir data
#data = data[-which(is.na(data$lat)|is.na(data$lon)|is.na(data$Temperature_F)|is.na(data$Humidity_.)|is.na(data$PM2.5_ATM_ug.m3)|data$PM2.5_ATM_ug.m3>1000),]

# Create separate list for each day
daily = list()
for (i in 1:78){
  temp = data.frame()
  for (j in 1:dim(data)[1]){
    if (as.numeric(substring(data$created_at[j],6,7)) == 7 & as.numeric(substring(data$created_at[j],9,10)) == i+14){
      temp = rbind.data.frame(temp,data[j,])
    }
    if (as.numeric(substring(data$created_at[j],6,7)) == 8 & as.numeric(substring(data$created_at[j],9,10)) == i-17){
      temp = rbind.data.frame(temp,data[j,])
    }
    if (as.numeric(substring(data$created_at[j],6,7)) == 9 & as.numeric(substring(data$created_at[j],9,10)) == i-48){
      temp = rbind.data.frame(temp,data[j,])
    }
  }
  daily[[i]] = temp
}

# Create data frame for before and after the fire
before=c()
for (i in 1:35){
  before = rbind.data.frame(before,daily[[i]])
}
before = as.data.frame(before %>% group_by(node) %>% summarize(mean = mean(PM2.5_ATM_ug.m3),sd = sd(PM2.5_ATM_ug.m3),lon=mean(lon),lat=mean(lat),count=n()))

after=data.frame()
for (i in 36:78){
  after = rbind.data.frame(after,daily[[i]])
}
after = as.data.frame(after %>% group_by(node) %>% summarize(mean = mean(PM2.5_ATM_ug.m3),sd = sd(PM2.5_ATM_ug.m3),lon=mean(lon),lat=mean(lat),count=n()))

# Read in epa data
setwd("~/Dropbox/Shen_et_al_2021/R code")
epa = read.csv("ad_viz_plotval_data.csv",header=T)
# Select the site in SF
epa = filter(epa,Site.ID=="60750005")
epa = dplyr::select(epa,Date,Site.ID,Daily.Mean.PM2.5.Concentration,SITE_LATITUDE,SITE_LONGITUDE)
colnames(epa) = c("Date","ID","mean","lat","lon")

# Set boundary for spatial plotting
boundary= make_bbox(data$lon, data$lat, f = 0.1)
map= get_stamenmap(boundary, zoom = 12, maptype = "terrain-lines")

# Figure 2.a
ggmap(map) + scalebar(x.min = -122.40, x.max = -122.36, y.min = 37.705, y.max = 37.72,dist = 2, dist_unit = "km",height=0.1,st.dist = 0.15,transform = TRUE, model = "WGS84")+ 
  geom_point(before, mapping = aes(lon,lat,size=mean,color=mean),alpha=0.9) + 
  scale_size_continuous(limits=c(0,130),breaks = c(0,25,50,75,100,125))+ 
  scale_color_continuous(limits=c(0,130),breaks = c(0,25,50,75,100,125),type = "viridis") +
  guides(color = guide_legend(), size = guide_legend()) +
  theme_grey(base_size = 25) + theme(legend.position = "none") +
  geom_point(epa,mapping = aes(lon,lat),size = 3) + geom_text(epa,mapping = aes(lon,lat),label="EPA",nudge_y = 0.003,size = 8)

# Figure 2.b
ggmap(map) + scalebar(x.min = -122.405, x.max = -122.365, y.min = 37.705, y.max = 37.72,dist = 2, dist_unit = "km",height=0.1,st.dist = 0.15,transform = TRUE, model = "WGS84")+ 
  geom_point(after, mapping = aes(lon,lat,size=mean,color=mean),alpha = 0.9) + 
  scale_size_continuous(limits=c(0,130),breaks = c(0,25,50,75,100,125)) +
  scale_color_continuous(limits=c(0,130),breaks = c(0,25,50,75,100,125),type = "viridis") +
  guides(color = guide_legend(), size = guide_legend()) +
  theme_grey(base_size = 25) + labs(size=expression(paste("PM2.5 ",mu,"g/m"^3,sep="")), color=expression(paste("PM2.5 ",mu,"g/m"^3,sep=""))) +
  geom_point(epa,mapping = aes(lon,lat),size = 3) + geom_text(epa,mapping = aes(lon,lat),label="EPA",nudge_y = 0.003,size = 8)
