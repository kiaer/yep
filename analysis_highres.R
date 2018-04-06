library(maps)
library(mapdata)
library(reshape)
library(ggplot2)
library(rasterVis)
library(lubridate)
library(raster)

### High resolution Data 

METnameH <- "METHigh.nc"
nrtnameH <- "METHighNRT.nc"

matten <- c(  0.66, 0.23, 0.03,-1.06,-0.01, 0.24,-0.27,-1.51,
              -0.50,-0.23, 0.37, 0.29, 0.13, 0.64, 1.11, 0.41, 0.52, 0.47, 0.67, 
              -0.36, 0.23, 0.36, 0.74, 0.82, 0.16, 0.21, 0.34,-0.04, 0.94, 0.48)


METH.b <- brick(METnameH)
nrtH.b <- brick(nrtnameH)

combH <- stack(METH.b, nrtH.b)

z <- list(getZ(METH.b), getZ(nrtH.b))  

z <- unlist(z) + ISOdate(year = 1981, 1, 1,0,0,0,tz="GMT") 
z <- format(z,format='%Y-%m-%d')

names(combH) <- z

nms <- expand.grid(paste0('X',1985:2014),c("03","04","05"
                                           ,"06"), 
                   c("01","02","03","04","05","06","07",
                     "08","09","10","11","12","13","14",
                     "15","16","17","18","19","20","21",
                     "22","23","24","25","26","27","28",
                     "29","30","31"
                   ))
nms <- apply(nms,1,function(x) paste0(x,collapse = '.'))

nms <- sort(nms)

r <- combH[[nms]]
r[r < -300] <- NA
r <- calc(r, fun = function(x) {x - 273.15})

indices = lapply(1:(30*4*30.5), function(x) {x %% 30 + 1})

r.mjmeanH <- stackApply(r, indices = sort(unlist(indices)), fun = function(x, na.rm){sum(x)},  na.rm = FALSE)

r.mjmeanH <- calc(r.mjmeanH, fun = function(x) {(x / 122)})

#r.test[r.test < 0] <- NA
nms <- expand.grid(paste0('X',1985:2014))
nms <- apply(nms,1,function(x) paste0(x,collapse = '.'))
nms <- sort(nms)
names(r.mjmeanH) <- nms

r.meanH <- calc(r.mjmeanH, mean)

r.anomH <- r.mjmeanH[[1]] - r.meanH
#plot(r.mjmean[[18]])
for (i in 2:30){
  temp <- r.mjmeanH[[i]] - r.meanH
  r.anomH <- stack(r.anomH, temp)
}

r.corrH <- stackApply(r.anomH, indices = c(1), fun = function(x, na.rm) {cor(matten, x)})

gplot(r.corrH)+geom_raster(aes(fill=value), interpolate = FALSE)+
  borders(fill="black",colour="black",size=2) +
  scale_fill_distiller(palette="YlOrRd",na.value="black")+ coord_quickmap(165:174.95, -44:-48, expand=FALSE)+xlim(165,175)+ylim(-48,-44)+
  xlab("Longitude (degrees)") + ylab("Latitude (degrees)") + #theme_bw()+
  labs(fill="Correlation")

lon.pts <- seq(169.5,169.5, by=1)
lat.pts <- rep(-47,length(lon.pts))

extract.pts <- cbind(lon.pts,lat.pts)

ann.extract <- extract(r.mjmeanH,extract.pts,method="bilinear")
ann.extractAno <- extract(r.anomH,extract.pts,method="bilinear")


x1 <- ann.extract[1:30]
x2 <- ann.extractAno[1:30]

matplot(1985:2014,x1,type="l",xlab="Year",ylab="SST (Celsius)")
matplot(1985:2014,x2,type="l",xlab="Year",ylab="Anomaly")
