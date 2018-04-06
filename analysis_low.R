library(maps)
library(mapdata)
library(reshape)
library(ggplot2)
library(rasterVis)
library(lubridate)
library(raster)

fname <- "MET.nc"
nrtname <- "METnrt.nc"

matten <- c(  0.66, 0.23, 0.03,-1.06,-0.01, 0.24,-0.27,-1.51,
              -0.50,-0.23, 0.37, 0.29, 0.13, 0.64, 1.11, 0.41, 0.52, 0.47, 0.67, 
              -0.36, 0.23, 0.36, 0.74, 0.82, 0.16, 0.21, 0.34,-0.04, 0.94, 0.48)


nrt.b <- brick(nrtname)
MET.b <- brick(fname, varname = "analysed_sst")
#comb.b <- mosaic(MET.b, nrt.b, fun = mean)
comb <- stack(MET.b, nrt.b)
z <- list(getZ(MET.b), getZ(nrt.b))  
#getZ(MET.b)
z = unlist(z) + ISOdatetime(year = 1981, 1, 1,0,0,0,tz="GMT") 

names(comb) <- year(z)
#comb
comb[["X1985.12"]]

ROI <- extent(165,175,-48,-44)


nms <- expand.grid(paste0('X',1985:2014),c("3","4","5"
                                           ,"6"))
nms <- apply(nms,1,function(x) paste0(x,collapse = '.'))

nms <- sort(nms)

r = comb[[nms]]
r[r < -300] <- NA

r.crop <- crop(r,ROI)

r.mean <- calc(r.crop, mean)

r.mean <- calc(r.mean, fun = function(x) {x - 273.15})

#r.mean[]
indices = lapply(1:(30*4), function(x) {x %% 30 + 1})

r.mjmean <- stackApply(r.crop, indices = sort(unlist(indices)), fun = sum, na.rm = TRUE)
#r.mjmean[[30]][]
r.mjmean <- calc(r.mjmean, fun = function(x) {(x / 4) - 273.15})
r.mjmean[r.mjmean < -100] <- NA


r.anom <- r.mjmean[[1]] - r.mean
#plot(r.mjmean[[18]])
for (i in 2:30){
  temp <- r.mjmean[[i]] - r.mean
  r.anom <- stack(r.anom, temp)
}

r.corr <- stackApply(r.anom, indices = c(1), fun = function(x, na.rm) {cor(matten, x)})

gplot(r.corr)+geom_raster(aes(fill=value), interpolate = FALSE)+
  borders(fill="black",colour="black",size=2) +
  scale_fill_distiller(palette="YlOrRd",na.value="black")+ coord_quickmap(165:174.95, -44:-48, expand=FALSE)+xlim(165,175)+ylim(-48,-44)+
  xlab("Longitude (degrees)") + ylab("Latitude (degrees)") + #theme_bw()+
  labs(fill="Correlation")