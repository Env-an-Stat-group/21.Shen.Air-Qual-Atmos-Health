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

beforeinterpolation = interpolation(before,data.grid.interp,inside.sf)

# Get population data. Can also be directly loaded.
sf_pop = get_acs(geography = "tract", 
                  variables = "B01003_001", 
                  state = "CA",
                  county = c("San Francisco"),
                  keep_geo_vars = T,
                  geometry = TRUE) 
load("sf_pop.RData")
# Find the centroid for each census tract
sf_pop$center = st_centroid(sf_pop$geometry)
sf_pop$lon = unlist(sf_pop$center)[c(T,F)]
sf_pop$lat = unlist(sf_pop$center)[c(F,T)]
# Exclude areas that are not included in the study
sf_pop = sf_pop[-c(which.max(sf_pop$lat),which.min(sf_pop$lon),dim(sf_pop)[1]),]

# Daily baseline mortality from non-communicable diseases
rate= 779.99 / 100000 / 365

# Create columns for calculating mortality rate in each census tract
sf_pop$mortalityrate= rep(0,dim(sf_pop)[1])
# Create vectors for daily mortality rate and its upperbound and lowerbound
date = c()
sum.daily = c()
lowerbound = c()
upperbound = c()

# Morality calculation
for (k in 36:78){
  temp = as.data.frame(daily[[k]])
  colnames(temp)[9] = "mean"
  tempinterpolation = interpolation(temp,data.grid.interp,inside.sf)
  mortality = c()
  mortalitylower = c()
  mortalityupper = c()
  for (i in 1:dim(sf_pop)[1]){
    sfpoly = unlist(sf_pop[i,]$geometry)
    inside = point.in.polygon(point.x = tempinterpolation$lon, point.y = tempinterpolation$lat, pol.x = sfpoly[1:length(sfpoly)/2] , pol.y =sfpoly[(length(sfpoly)/2 +1) :length(sfpoly)])
    mean.diff = mean((tempinterpolation$pred-beforeinterpolation$pred)[which(inside!=0)])
    mortality[i] = rate*sf_pop$estimate[i]*(exp(0.00094*mean.diff)-1)/exp(0.00094*mean.diff)
    mortalitylower[i] = rate*sf_pop$estimate[i]*(exp(0.00073*mean.diff)-1)/exp(0.00073*mean.diff)
    mortalityupper[i] = rate*sf_pop$estimate[i]*(exp(0.00116*mean.diff)-1)/exp(0.00116*mean.diff)
  }
  date[k-35] = substring(temp$created_at[1],6,10)
  sum.daily[k-35] = sum(mortality)
  lowerbound[k-35] = sum(mortalitylower)
  upperbound[k-35] = sum(mortalityupper)
  sf_pop$mortalityrate = sf_pop$mortalityrate + mortality
}

# Mortality estimates
sum(sum.daily)
sum(lowerbound)
sum(upperbound)

time.series=cbind.data.frame(date,sum.daily,lowerbound,upperbound)
time.series$sum.daily[which(time.series$sum.daily < 0)] =0 
time.series$lowerbound[which(time.series$lowerbound < 0)] =0 
time.series$upperbound[which(time.series$upperbound < 0)] =0 
time.series$date = as.POSIXct(time.series$date,"%m-%d",tz = "", origin = time.series$date[1])

# Figure 5.a
ggplot(time.series, aes(date,group = 1)) + 
  geom_line(aes(y = sum.daily),color="red",size=1.2) + 
  geom_line(aes(y = upperbound),color="red",size=1.2,linetype = "dashed") +
  geom_line(aes(y = lowerbound),color="red",size=1.2,linetype = "dashed") +
  geom_ribbon(aes(x=date, ymax=upperbound, ymin=lowerbound), fill="pink", alpha=.5) +
  scale_x_datetime(breaks=seq(min(time.series$date),max(time.series$date),by="5 days"),labels=date_format("%m/%d")) +
  ylab("Deaths per day") + theme_grey(base_size = 20)+ theme(legend.position = "none",axis.title.x=element_blank()) 

# Figure 5.b
ggplot()+geom_sf(data=sf_pop,aes(fill=mortalityrate/estimate*100000)) +
  scale_fill_viridis_c(limits=c(0.5,3.5),breaks = c(1,2,3)) + 
  labs(x="lon",y="lat",fill='Mortality')+ theme_grey(base_size = 18)+ 
  ggsn::scalebar(x.min = -122.38, x.max = -122.36,y.min = 37.70, y.max = 37.71,location = "bottomright",dist = 2, dist_unit = "km",height=0.2,st.dist = 0.3,transform = TRUE, model = "WGS84")
