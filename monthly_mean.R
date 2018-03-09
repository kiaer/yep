library('raster')

monthly_mean <- function(lon.pts, lat.pts, HadISST.b, ROI ){
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
  r.crop <- crop(r,ROI)
  
  extract.pts <- cbind(lon.pts,lat.pts)
  ext <- extract(r.crop,extract.pts,method="bilinear")
  
  ann.extract <- extract(r.crop,extract.pts,method="bilinear")
  
  years <- 2014 - 1954
  months <- years * 12
  means <- list()
  
  sum <- 0
  
  for (i in 1:12){
    for (j in 1:years){
      pos <- i + ((j - 1) * 12)
      sum <- sum + head(ann.extract[pos])
    }
    means[i] <- sum / years
    sum <- 0 
  }
  
  yrly = Reduce(`+`, means) / 12
  mj.means <- mean(unlist(means[3:6]))
  
  return(c(means,mj.means,yrly))
}