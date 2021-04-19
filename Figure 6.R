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

# Read in PurpleAir data
setwd("C:/Users/Peng Shen/Dropbox/Shen_et_al_2021/R code/Daily")
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

# Cut the grid to fit SF's shapefile
geo.box = c(xmin=-122.520, xmax=-122.355, ymin=37.705, ymax=37.816)
gshhg.l1 = sf::read_sf("C:/Users/Peng Shen/Downloads/shapefile/GSHHS_f_L1.shp") %>% st_crop(geo.box)
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

# Get population data
sf_pop = get_acs(geography = "tract", 
                 variables = "B01003_001", 
                 state = "CA",
                 county = c("San Francisco"),
                 keep_geo_vars = T,
                 geometry = TRUE) 
# Exclude areas that are not included in the study
sf_pop = sf_pop[-c(82,61,197),]
# Find the centroid for each census tract
sf_pop$center = st_centroid(sf_pop$geometry)
sf_pop$lon = unlist(sf_pop$center)[c(T,F)]
sf_pop$lat = unlist(sf_pop$center)[c(F,T)]

# Daily baseline mortality from non-communicable diseases
rate= 779.99 / 100000 / 365

# Create columns for calculating mortality rate in each census tract
sf_pop$mortalityrate= rep(0,dim(sf_pop)[1])
# Create vectors for daily mortality rate and its upperbound and lowerbound
date = c()
sum.daily = c()

# Morality calculation for Purple Air data
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
    mortality[i] = rate*sf_pop$estimate[i]*(exp(0.00104*mean.diff)-1)/exp(0.00104*mean.diff)
  }
  date[k-35] = substring(temp$created_at[1],6,10)
  sum.daily[k-35] = sum(mortality)
  sf_pop$mortalityrate = sf_pop$mortalityrate + mortality
}

# Read in EPA data
setwd("C:/Users/Peng Shen/Dropbox/Shen_et_al_2021/R code")
epa = read.csv("ad_viz_plotval_data.csv",header=T)
# Select the site in SF
epa = filter(epa,Site.ID=="60750005")
epa = dplyr::select(epa,Date,Site.ID,Daily.Mean.PM2.5.Concentration,SITE_LATITUDE,SITE_LONGITUDE)
colnames(epa) = c("Date","ID","mean","lat","lon")

# Morality calculation for EPA data
sf_pop$mortalityrate.epa= rep(0,dim(sf_pop)[1])
epa.daily = rep(0,dim(sf_pop)[1])
for (k in 1:43){
  temp = as.data.frame(epa[231+k,])
  diff = temp$mean-mean(epa[197:231,]$mean)
  for (i in 1:dim(sf_pop)[1]){
    sf_pop$mortalityrate.epa[i] = sf_pop$mortalityrate.epa[i]+rate*sf_pop$estimate[i]*(exp(0.00104*diff)-1)/exp(0.00104*diff)
  }
}

# Calculate the difference of mortality estimates between Purple Air and EPA
sf_pop$difference = sf_pop$mortalityrate - sf_pop$mortalityrate.epa

# Figure 6
ggplot()+geom_sf(data=sf_pop,aes(fill=difference/estimate*100000)) +
  scale_fill_gradientn(colours=c("blue","lightblue","white","pink","red"),breaks=c(-2,-1,0,1,2),limits=c(-2,2)) +
  labs(x="lon",y="lat",fill='')+ theme_grey(base_size = 18)+ 
  ggsn::scalebar(x.min = -122.38, x.max = -122.36,y.min = 37.70, y.max = 37.71,location = "bottomright",dist = 2, dist_unit = "km",height=0.2,st.dist = 0.3,transform = TRUE, model = "WGS84")
