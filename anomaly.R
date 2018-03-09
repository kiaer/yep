library('raster')

anomaly <- function(monthly.mean, yearly.mean, mj.mean, year, ROI, HadISST.b, lat.pts, lon.pts){
  
  nms <- expand.grid(paste0('X',year:year),c("01","02","03","04","05"
                                             ,"06","07","08","09","10","11","12"),'16')
  nms <- apply(nms,1,function(x) paste0(x,collapse = '.'))
  
  
  nmsfeb <- expand.grid(paste0('X',year:year),c("02"),'15')
  nmsfeb <- apply(nmsfeb,1,function(x) paste0(x,collapse = '.'))
  
  nms <- append(nms, nmsfeb)
  nms <- sort(nms)
  
  r <- HadISST.b[[nms]]
  r[r < -300] <- NA
  
  r.crop <- crop(r,ROI)
  
  extract.pts <- cbind(lon.pts,lat.pts)
  ext <- extract(r.crop,extract.pts,method="bilinear")
  
  ann.extract <- extract(r.crop,extract.pts,method="bilinear")
  
  yearly.ano <- mean(ann.extract)
  
  monthly.ano <- lapply(1:12, function(x) ann.extract[x] - monthly.mean[[x]])
  
  mj.ano <- mean(ann.extract[3:6]) - mj.mean
  return(c(monthly.ano, yearly.ano, mj.ano)) 

}