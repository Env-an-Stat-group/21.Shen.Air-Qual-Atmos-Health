rm(list=ls())
# Load packages
library(tidyverse)
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

# Preprocessed PurpleAir date can be directly loaded
load("purple.RData")
# Read in PurpleAir data
setwd("~/Dropbox/Shen_et_al_2021/R code/Daily")
list.filenames<-list.files()
data = data.frame()
for (i in 1:length(list.filenames)){
  temp = read.csv(list.filenames[i],header=T)
  if (dim(temp)[1] >= 5) {
    temp = temp[,1:9]
    starting = as.numeric(str_locate_all(pattern ='\\.',list.filenames[i])[[1]][1,2]) -2
    temp$lat = as.numeric(substring(list.filenames[i],starting,starting+8))
    temp$lon = as.numeric(substring(list.filenames[i],starting+9,starting+18))
    temp$node = i
    data = rbind.data.frame(data,temp)
  }
}
# Clean PurpleAir data
data = data[-which(is.na(data$lat)|is.na(data$lon)|is.na(data$Temperature_F)|is.na(data$Humidity_.)|is.na(data$PM2.5_ATM_ug.m3)|data$PM2.5_ATM_ug.m3>1000),]

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

# Fit the data with linear model
for (i in 1:78){
  daily[[i]]$PM2.5_ATM_ug.m3 = 10^(-0.39222+0.66337*log10(daily[[i]]$PM2.5_ATM_ug.m3)+0.01048*daily[[i]]$Humidity_.+0.00941*daily[[i]]$Temperature_F)
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

# Create grid for interpolation
lon.seq=seq(-122.52536,-122.35424,0.0001)
lat.seq=seq(37.69248,37.81824,0.0001)
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

# Interpolate for before fire 
beforeinterpolation = interpolation(before,data.grid.interp,inside.sf)
summary(beforeinterpolation$pred)
rng.before = range(c(19,28))
beforeinterpolation$pred[beforeinterpolation$pred>=rng.before[2]]=rng.before[2]
beforeinterpolation$pred[beforeinterpolation$pred<=rng.before[1]]=rng.before[1]

# Figure 4.a
ggplot(data=beforeinterpolation) + geom_raster(aes(x=lon, y=lat,fill=pred)) + 
  scale_fill_gradientn(limits=c(15,60),colours=viridis(10)) +labs(fill='') + 
  theme_grey(base_size = 18)+ theme(legend.position = "none") +
  theme(axis.text.x=element_text(size=18),axis.text.y=element_text(size=18))+ 
  scalebar(x.min = -122.38, x.max = -122.35,y.min = 37.70, y.max = 37.71,location = "bottomright",dist = 2, dist_unit = "km",height=0.2,st.dist = 0.3,transform = TRUE, model = "WGS84")

# Interpolate for after fire 
afterinterpolation = interpolation(after,data.grid.interp,inside.sf)
summary(afterinterpolation$pred)
rng.after = range(c(46,57))
afterinterpolation$pred[afterinterpolation$pred>=rng.after[2]]=rng.after[2]
afterinterpolation$pred[afterinterpolation$pred<=rng.after[1]]=rng.after[1]

# Figure 4.b
ggplot(data=afterinterpolation) + geom_raster(aes(x=lon, y=lat,fill=pred)) + 
  scale_fill_gradientn(limits=c(15,60),colours=viridis(10)) +labs(fill='')+ 
  theme_grey(base_size = 18) + 
  theme(axis.text.x=element_text(size=18),axis.text.y=element_text(size=18))+
  scalebar(x.min = -122.38, x.max = -122.35,y.min = 37.70, y.max = 37.71,location = "bottomright",dist = 2, dist_unit = "km",height=0.2,st.dist = 0.3,transform = TRUE, model = "WGS84")
