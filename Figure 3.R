rm(list=ls())
# Load packages
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggmap)
library(stringr)
library(ggsn)
library(googleway)
library(geosphere)
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

# Find the closest Purple Air sensor with complete record to EPA sensor 
count_by_node = data %>% group_by(node) %>% summarise(count = n())
complete_record = count_by_node[count_by_node$count==78,]$node
dist = c()
for (i in 1:length(complete_record)){
  dist = c(dist,as.numeric(distm(c(filter(data,node == complete_record[i])[1,]$lon, filter(data,node == complete_record[i])[1,]$lat), c(epa$lon[1], epa$lat[1]), fun = distHaversine)))
}
complete_record[which.min(dist)]# The Purple Air sensor selected is node 513

# Create EPA records for before, during the fire and combined records
epa.before = epa[197:231,]
epa.after = epa[232:274,]
rownames(epa.before) = NULL
rownames(epa.after) = NULL
epa.before$purple = filter(data,node==513)$PM2.5_ATM_ug.m3[1:35]
epa.after$purple = filter(data,node==513)$PM2.5_ATM_ug.m3[36:78]
epa.before$humidity = filter(data,node==513)$Humidity_.[1:35]
epa.after$humidity = filter(data,node==513)$Humidity_.[36:78]
epa.before$temperature = filter(data,node==513)$Temperature_F[1:35]
epa.after$temperature = filter(data,node==513)$Temperature_F[36:78]
epa.before$mean=log10(epa.before$mean)
epa.before$purple=log10(epa.before$purple)
epa.after$mean=log10(epa.after$mean)
epa.after$purple=log10(epa.after$purple)
combined = rbind.data.frame(epa.before,epa.after)

# Fit linear regression model with data from all the analyzed period
mod.combined = lm(mean~purple+humidity+temperature,data=combined)
summary(mod.combined)
combined$fitted = mod.combined$fitted.values

# Figure 3
ggplot() + 
  geom_point(combined[1:35,],mapping=aes(mean,fitted,color="Before wildfire")) +
  geom_point(combined[36:78,],mapping=aes(mean,fitted,color="During wildfire"))+
  xlab("EPA") + ylab("Fitted") + theme_grey(base_size = 20) +
  scale_color_manual(name = NULL,labels = c("Before wildfire","During wildfire","Combined"),breaks = c("Before wildfire", "During wildfire", "Combined"),values = c("blue", "green","red"))+
  scale_x_continuous(limits=c(0,2.25),breaks=c(0,1,2),label=c(1,10,100)) + scale_y_continuous(limits=c(0,2.25),breaks=c(0,1,2),label=c(1,10,100))
