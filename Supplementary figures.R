rm(list=ls())
# Load packages
library(tidyverse)
library(tidycensus)
library(ggmap)
library(stringr)
library(ggsn)
library(gstat)
library(sp)
library(sf)
library(spatialEco)
library(geosphere)
library(viridis)
library(fields)
library(scales)

################################################
################################################
################################################
# Get population data. Can also be directly loaded.
#sf_pop = get_acs(geography = "tract", 
#                 variables = "B01003_001", 
#                 state = "CA",
#                 county = c("San Francisco"),
#                 keep_geo_vars = T,
#                 geometry = TRUE) 
load("sf_pop.RData")
# Find the centroid for each census tract
sf_pop$center = st_centroid(sf_pop$geometry)
sf_pop$lon = unlist(sf_pop$center)[c(T,F)]
sf_pop$lat = unlist(sf_pop$center)[c(F,T)]
# Exclude areas that are not included in the study
sf_pop = sf_pop[-c(which.max(sf_pop$lat),which.min(sf_pop$lon),dim(sf_pop)[1]),]

# Figure S1
ggplot()+geom_sf(data=sf_pop,aes(fill=estimate))+scale_fill_viridis_c(breaks=seq(2000,16000,by=4000)) +
  labs(x="lon",y="lat",fill='Population count')+ 
  theme_grey(base_size = 18)+ 
  scalebar(x.min = -122.38, x.max = -122.36, y.min = 37.70, y.max = 37.71,location = "bottomright",dist = 2, dist_unit = "km",height=0.2,st.dist = 0.3,transform = TRUE, model = "WGS84")

################################################
################################################
################################################
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

# Read in EPA data
setwd("~/Dropbox/Shen_et_al_2021/R code/")
epa = read.csv("ad_viz_plotval_data.csv",header=T)
# Select the site in SF
epa = filter(epa,Site.ID=="60750005")
epa = dplyr::select(epa,Date,Site.ID,Daily.Mean.PM2.5.Concentration,SITE_LATITUDE,SITE_LONGITUDE)
colnames(epa) = c("Date","ID","mean","lat","lon")

# Select epa data from July 15th to September 30th
time.series = epa[197:274,]

# Add the fitted records from the closest Purple Air sensors (node id is 513)
temp = filter(data,node==513)
time.series$purple = 10^(-0.39222+0.66337*log10(temp$PM2.5_ATM_ug.m3)+0.01048*temp$Humidity_.+0.00941*temp$Temperature_F)

# Figure S2
time.series$Date = as.POSIXct(time.series$Date,"%m/%d/%Y",tz = "", origin = time.series$Date[1])
ggplot(time.series, aes(time.series$Date,group = 1)) + labs(color='Sites',y="PM2.5 Concentration")+ theme_grey(base_size = 18)+
  geom_line(aes(y = mean, color = "EPA"),size=1.2) + 
  geom_line(aes(y = purple, color = "Purple"),size=1.2) +
  scale_x_datetime(breaks=seq(min(time.series$Date),max(time.series$Date),by="7 days"),labels=date_format("%m/%d"))+
  theme(legend.position = c(.95, .95),legend.justification = c("right", "top"),legend.box.just = "right",legend.margin = margin(6, 6, 6, 6),axis.title.x=element_blank())

################################################
################################################
################################################
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

# Find the peak day
peak=c()
for (i in 36:78){
  peak[i-35] = mean(daily[[i]]$PM2.5_ATM_ug.m3)
}
which.max(peak)+35

# Figure S3
boundary= make_bbox(data$lon, data$lat, f = 0.1)
map= get_stamenmap(boundary, zoom = 12, maptype = "terrain-lines")
ggmap(map) + scalebar(x.min = -122.405, x.max =  -122.365, y.min = 37.705, y.max = 37.72,dist = 2, dist_unit = "km",height=0.1,st.dist = 0.15,transform = TRUE, model = "WGS84")+
  geom_point(daily[[59]], mapping = aes(x=lon,y=lat,size=PM2.5_ATM_ug.m3,color=PM2.5_ATM_ug.m3),alpha = 0.9)+ scale_size_continuous(limits=c(0,300),breaks = c(0,50,100,150,200,250,300))+
  scale_color_continuous(limits=c(0,300),breaks = c(0,50,100,150,200,250,300),type = "viridis") +
  guides(color = guide_legend(), size = guide_legend()) +
  theme_grey(base_size = 20) + labs(size='Peak Day 09/11', color='Peak Day 09/11') +
  geom_point(epa,mapping = aes(lon,lat),size = 3) + geom_text(epa,mapping = aes(lon,lat),label="EPA",nudge_y = 0.003,size = 8)

################################################
################################################
################################################
# Fit the data with linear model
for (i in 1:78){
  daily[[i]]$PM2.5_ATM_ug.m3 = 10^(-0.39222+0.66337*log10(daily[[i]]$PM2.5_ATM_ug.m3)+0.01048*daily[[i]]$Humidity_.+0.00941*daily[[i]]$Temperature_F)
}

# Create grid for interpolation
lon.seq=seq(-122.52536,-122.35424,0.001)
lat.seq=seq(37.69248,37.81824,0.001)
data.grid.interp=data.frame(expand.grid(lon.seq, lat.seq))
names(data.grid.interp)=c("lon","lat")
coordinates(data.grid.interp)=~lon+lat
data.grid.interp = SpatialPointsDataFrame(data.grid.interp, data.frame(ID=1:length(data.grid.interp)))

# Cut the grid to fit SF's shape
# Following two lines are preprocessing steps. SF's shape can be directly loaded.
# geo.box = c(xmin=-122.520, xmax=-122.355, ymin=37.705, ymax=37.816)
# gshhg.l1 = sf::read_sf("C:/Users/Peng Shen/Downloads/shapefile/GSHHS_f_L1.shp") %>% st_crop(geo.box)
load("sf_shape.RData")
sfpoly = unlist(gshhg.l1[1,]$geometry)
inside.sf = point.in.polygon(point.x =as.vector(coordinates(data.grid.interp)[,1]), point.y = as.vector(coordinates(data.grid.interp)[,2]), pol.x = sfpoly[1:245] , pol.y =sfpoly[246:490])

interpolation= function(data,data.grid.interp,inside.sf){
  trend.spline=Tps(cbind(data$lon,data$lat), data$mean,lambda=1e-3)
  data.tps=predict(trend.spline,cbind(data.grid.interp$lon,data.grid.interp$lat))
  new=c()
  new$pred = data.tps[which(inside.sf==1),]
  new$lon=data.grid.interp$lon[which(inside.sf==1)]
  new$lat=data.grid.interp$lat[which(inside.sf==1)]
  return(as.data.frame(new))
}

# Find daily number of population exposed to PM 2.5 concentration > 35
date = c()
populationinfluenced = rep(0,43)
# This loop takes a long time
for (j in 36:78){
  colnames(daily[[j]])[9] = "mean"
  tempdata = interpolation(daily[[j]],data.grid.interp,inside.sf)
  date[j-35] = substring(tempdata$created_at[1],6,10)
  for (i in 1:dim(sf_pop)[1]){
    # Find the grid point closest to each census tract's centroid
    sfpoly.temp = unlist(sf_pop[i,]$geometry)
    inside.temp = point.in.polygon(point.x = tempdata$lon, point.y = tempdata$lat, pol.x = sfpoly.temp[1:length(sfpoly.temp)/2] , pol.y =sfpoly.temp[(length(sfpoly.temp)/2 +1) :length(sfpoly.temp)])
    tract = tempdata[which(inside.temp!=0),]
    dist =c()
    for (k in 1:length(which(inside.temp!=0))){
      dist = c(dist,as.numeric(distm(c(tract$lon[k], tract$lat[k]), c(sf_pop$lon[i], sf_pop$lat[i]), fun = distHaversine)))
    }
    index = sort(dist, index.return=TRUE, decreasing=FALSE)$ix[1]
    mean.inside = tract$pred[index]
    if(mean.inside > 35){
      populationinfluenced[j-35] = populationinfluenced[j-35] + sf_pop$estimate[i]
    }
  }
}
for (j in 36:78){
  date[j-35] = substring(daily[[j]]$created_at[1],6,10)
}
date = as.POSIXct(date,"%m-%d",tz = "", origin = date[1])
populationinfluenced = cbind.data.frame(date,populationinfluenced)

# Figure S4
ggplot(populationinfluenced) + 
  geom_line(aes(x = date,y = populationinfluenced),color="red",size=1.2) + 
  scale_x_datetime(breaks=seq(min(date),max(date),by="5 days"),labels=date_format("%m/%d")) +
  theme_grey(base_size = 20)+ theme(legend.position = "none",axis.title.x=element_blank(),axis.title.y =element_blank()) 
