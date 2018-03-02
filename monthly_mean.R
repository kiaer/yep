library('raster')

fname <- "~/yep/HadISST_sst.nc"
HadISST.b <- brick(fname)

r <- HadISST.b[[1776]]
r[r < -300] <- NA

test <- HadISST.b[["X1961.02.16"]]
HadISST.b$

plot(r)
ROI <- extent(165,175,-48,-44)
ROI
r.crop <- crop(r,ROI)
plot(r.crop)

lon.pts <- seq(169,170, by=1)
lat.pts <- rep(-47,length(lon.pts))
points(lon.pts,lat.pts,pch=4,col="red")

nms <- expand.grid(paste0('X',1954:2014),c("01","02","03","04","05"
                                           ,"06","07","08","09","10","11","12"),'16')
nms <- apply(nms,1,function(x) paste0(x,collapse = '.'))


nmsfeb <- expand.grid(paste0('X',1954:2014),c("02"),'15')
nmsfeb <- apply(nmsfeb,1,function(x) paste0(x,collapse = '.'))
nmsfeb

nms <- append(nms, nmsfeb)
nms <- sort(nms)

r <- HadISST.b[[nms]]
r[r < -300] <- NA

ROI <- extent(165,175,-48,-44)
r.crop <- crop(r,ROI)
plot(r.crop$X2014.01.16)

r.crop

lon.pts <- seq(169,169, by=1)
lat.pts <- rep(-47,length(lon.pts))
points(lon.pts,lat.pts,pch=4,col="red")

extract.pts <- cbind(lon.pts,lat.pts)
ext <- extract(r.crop,extract.pts,method="bilinear")


ann.extract <- extract(r.crop,extract.pts,method="bilinear")
head(ann.extract)
length(head(ann.extract))

years = 2014 - 1954
months = years * 12
means = list()

sum = 0

for (i in 1:12){
  for (j in 1:years){
    test = test + 2
    pos = i + ((j - 1) * 12)
    sum = sum + head(ann.extract[pos])
  }
  means[i] = sum / years
  sum = 0
}

plot(1:12, means)
yrly = Reduce(`+`, means) / 12

means
yrly
