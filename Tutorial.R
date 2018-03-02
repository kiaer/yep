library("raster")

fname <- "~/yep/HadISST_sst.nc"
HadISST.b <- brick(fname)

HadISST.b

plot(HadISST.b[[1]])
r <- HadISST.b[[1776]]
plot(r)
r[r < -300] <- NA

plot(r)
ROI <- extent(150,180,-60,-30)
ROI
r.crop <- crop(r,ROI)
plot(r.crop)

lon.pts <- seq(160,180,by=0.5)
lat.pts <- rep(-46,length(lon.pts))
plot(r.crop)
points(lon.pts,lat.pts,pch=4,col="red")

extract.pts <- cbind(lon.pts,lat.pts)
ext <- extract(r.crop,extract.pts,method="bilinear")

ext
plot(lon.pts,ext,type="b",pch=2,xlab="Longitude",ylab="SST")

b <- HadISST.b[[1765:1776]]  #Most recent year
b
ann.extract <- extract(b,extract.pts,method="bilinear")
head(ann.extract)

matplot(lon.pts,ann.extract,type="l",xlab="Longitude",ylab="SST")
