library('raster')
source('monthly_mean.R')
source('anomaly.R')

fname <- "HadISST_sst.nc"
HadISST.b <- brick(fname)
#0.34,-1.14, 0.19,
matten <- c(  0.66, 0.23, 0.03,-1.06,-0.01, 0.24,-0.27,-1.51,
            -0.50,-0.23, 0.37, 0.29, 0.13, 0.64, 1.11, 0.41, 0.52, 0.47, 0.67, 
            -0.36, 0.23, 0.36, 0.74, 0.82, 0.16, 0.21, 0.34,-0.04, 0.94, 0.48)


ROI <- extent(165,175,-48,-44)
lon.pts <- seq(169.5,169.5, by=1)
lat.pts <- rep(-46.5,length(lon.pts))
r <- HadISST.b[[1776]]
r
r[r < -300] <- NA

r.crop <- crop(r,ROI)
plot(r.crop)
points(lon.pts,lat.pts,pch=4,col="red")


m <- monthly_mean(lon.pts, lat.pts, HadISST.b, ROI)
monthly.mean = m[1:12]
yearly.mean = m[[13]]
mj.mean = m[[14]]

year = 1981

a <- anomaly(monthly.mean, yearly.mean, mj.mean, year, ROI, HadISST.b, lat.pts, lon.pts)[13]

mj.timeseries <- lapply(1:33, function(x) {
  anomaly(monthly.mean, yearly.mean, mj.mean, year + x, ROI, HadISST.b, lat.pts, lon.pts)[14]
})
an.timeseries <- lapply(1:33, function(x) {
  anomaly(monthly.mean, yearly.mean, mj.mean, year + x, ROI, HadISST.b, lat.pts, lon.pts)[13]
})
cor(matten, unlist(mj.timeseries))

unlist(mj.timeseries)

plot(1982:2014, unlist(mj.timeseries))

plot(1982:2014, unlist(an.timeseries))

library('lubridate')

fname <- "MET.nc"
nrtname <- "METnrt.nc"

nrt.b <- brick(nrtname)
MET.b <- brick(fname, varname = "analysed_sst")
#comb.b <- mosaic(MET.b, nrt.b, fun = mean)
comb <- stack(MET.b, nrt.b)
z <- list(getZ(MET.b), getZ(nrt.b))  
getZ(MET.b)
z = unlist(z) + ISOdatetime(year = 1981, 1, 1,0,0,0,tz="GMT") 

comb@z <- list(z)

#z <- months(z)
comb@z

names(comb) <- year(z)
comb
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
plot(r.mjmean[[18]])
for (i in 2:30){
  temp <- r.mjmean[[i]] - r.mean
  r.anom <- stack(r.anom, temp)
}

r.anom
r.mjmean

r.corr <- stackApply(r.anom, indices = c(1), fun = function(x, na.rm) {cor(matten, x)})


r.mean[640]
r.mjmean[[1]][640]
plot(r.mjmean[[3]])
plot(r.mean)

#arg <- list(at=c(0.12,0.5,0.87), labels=c("Low","Med.","High"))
library(maps)
library(mapdata)
library(reshape)
library(ggplot2)
library(rasterVis)



plot(r.corr, xlab="Longitude", ylab="Latitude", legend.args=list(text='Correlation', side=4, font=2, line=2.5, cex=0.8))#lubridate
rcorr.dat <- data.frame(coordinates(r.corr),as.data.frame(r.corr))
head(rcorr.dat)

plt.dat <- melt(rcorr.dat,id.vars = c("x","y"))
head(plt.dat)
ggplot(plt.dat,aes(x=x,y=y,fill=value))+
  geom_raster()+borders(fill="black",colour="black") + facet_wrap(~variable)+
  scale_fill_distiller(palette="RdPu")+ coord_quickmap()

gplot(r.corr, xlab="lat")+geom_raster(aes(fill=value))+
  borders(fill="black",colour="black") + facet_wrap(~variable)+
  scale_fill_distiller(palette="RdYlBu")+ coord_quickmap(165:175, -44:-48)+xlim(165,175)+ylim(-48,-44)+
  xlab("Longitude") + ylab("Latitude")


nrtnameH <- "METHigh.nc"

nrtH.b <- brick(nrtnameH)

plot(nrtH.b[[1]])

#col=terrain.colors(2),