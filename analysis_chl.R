### Chl analysis
library(maps)
library(mapdata)
library(reshape)
library(ggplot2)
library(rasterVis)
library(lubridate)
library(raster)
require(gridExtra)

chlname <- "chl_high.nc"

chl.b <- brick(chlname)

#chl.b[is.na(chl.b)] <- 0
plot(chl.b[[195]])
chl.b[[195]]
ROI <- extent(170.3, 170.6,-46.2,-45.9)

nms <- expand.grid(paste0('X',1998),c("01"), 
                   c("01","02","03","04","05","06","07",
                     "08","09","10","11","12","13","14",
                     "15","16","17","18","19","20","21",
                     "22","23","24","25","26","27","28",
                     "29","30","31"
                   ))
nms <- apply(nms,1,function(x) paste0(x,collapse = '.'))

nms <- sort(nms)

r <- chl.b[[nms]]
r.crop <- crop(chl.b, ROI)
#r.crop[["X1998.01.01"]]
#r.crop[[1]]
#chl.b[[nms]]
r.mean <- calc(r.crop, mean, na.rm = TRUE)
#r.crop
plot(r.mean)
#plot(r.mjmean[[18]])
for (j in 1998:2016){
  for (i in 1:12){
    if (i > 9) {
      mth <- toString(i)
    }else{
    mth <- paste0("0", i)
    }
    nms <- expand.grid(paste0('X',toString(j)),mth, 
                       c("01","02","03","04","05","06","07",
                         "08","09","10","11","12","13","14",
                         "15","16","17","18","19","20","21",
                         "22","23","24","25","26","27","28",
                         "29","30","31"
                       ))
    nms <- apply(nms,1,function(x) paste0(x,collapse = '.'))
    
    nms <- sort(nms)
    r <- r.crop[[nms]]
    
    temp <- calc(r, mean, na.rm = TRUE)
    r.mean <- stack(r.mean, temp)
  }
}
plot(r.mean[[2]])
plot(r.mean)