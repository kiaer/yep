library("raster")

fname <- "~/yep/HadISST_sst.nc"
HadISST.b <- brick(fname)

HadISST.b

plot(HadISST.b[[1]])
r <- HadISST.b[[1776]]
plot(r)
r[r < -300] <- NA

plot(r)
ROI <- extent(165,175,-48,-44)
ROI
r.crop <- crop(r,ROI)
plot(r.crop)

lon.pts <- seq(169,170, by=1)
lat.pts <- rep(-47,length(lon.pts))
plot(r.crop)
points(lon.pts,lat.pts,pch=4,col="red")

extract.pts <- cbind(lon.pts,lat.pts)
ext <- extract(r.crop,extract.pts,method="bilinear")

ext
plot(lon.pts,ext,type="b",pch=2,xlab="Longitude",ylab="SST")

b <- HadISST.b[["1954":"2014"]]  #Most recent year
b
ann.extract <- extract(b,extract.pts,method="bilinear")
head(ann.extract)

matplot(lon.pts,ann.extract,type="l",xlab="Longitude",ylab="SST")

