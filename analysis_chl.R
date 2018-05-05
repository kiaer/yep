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

plot(chl.b[[100]])
r.crop <- crop(chl.b, ROI)
r.crop <- calc(r.crop, function(x){log(x)})
plot(r.crop[[1]])
#r.crop[[1]]
#chl.b[[nms]]
r.mean <- calc(r.crop, mean, na.rm = TRUE)
r.mean <- stack()
#r.crop
plot(r.mean)
#plot(r.mjmean[[18]])
j <- 1998
while(j < 2016){
  for (i in 10:12){
    mth <- toString(i)
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
  
  j <- j + 1
  for (i in 1:5){
    mth <- paste0("0", i)
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

# for (j in 1998:2016){
#   for (i in 10:17){
#     if (i%%13 > 9) {
#       mth <- toString(i)
#     }else{
#     mth <- paste0("0", (i%%13)+1)
#     }
#     mth
#     nms <- expand.grid(paste0('X',toString(j)),mth, 
#                        c("01","02","03","04","05","06","07",
#                          "08","09","10","11","12","13","14",
#                          "15","16","17","18","19","20","21",
#                          "22","23","24","25","26","27","28",
#                          "29","30","31"
#                        ))
#     nms <- apply(nms,1,function(x) paste0(x,collapse = '.'))
#     
#     nms <- sort(nms)
#     r <- r.crop[[nms]]
#     
#     temp <- calc(r, mean, na.rm = TRUE)
#     r.mean <- stack(r.mean, temp)
#   }
# }

plot(r.mean)
tot <- lapply(1:144, function(x){sum(r.mean[[x]][], na.rm=TRUE) / 48
})
tot <- lapply(tot, function(x){exp(x)})
tot2 <- matrix(unlist(tot), ncol = 8, byrow = TRUE)
tot1 <- matrix(unlist(tot), ncol = 18, byrow = FALSE)
plot(1:7,tot[1:7], type="l")
plot(1:7,tot[8:14], type="l")
#matplot(tot)
matplot(tot1, type = c("b"),pch=1,col = 1:8)
library(matlib)
require(graphics); require(utils)


yrs <- unlist(lapply(1998:2015, function(x){toString(x)}))

rownames(tot2) <- yrs
totdist <- dist(tot2)
hcd <- as.dendrogram(hclust(totdist))

hc <- hclust(totdist)
colors = c("red", "blue", "green", "black")
clus4 = cutree(hc, h = 1)

plot(hcd, xlab="Years grouped by cluster", ylab = "Height", horiz = FALSE)

months = c("oct", "nov", "dec", "jan", "feb", "mar", "apr", "may")
test <- matrix(unlist(c(tot1[,2000-1997], tot1[,2003-1997], tot1[,2004-1997], tot1[,2011-1997])), ncol=4, byrow = FALSE)
rownames(test) <- months
matplot(test, type = c("b"),pch=1, col=1:4, xlab = "Month", ylab = "Average chl concentration", xaxt="n",cex.lab=1.5, cex.axis=1.5, cex.main=1.5, cex.sub=1.5)
axis(1,at=1:8,labels=rownames(test), cex.axis=1.5)
legend("topright", inset=.00, legend=c("2000/2001", "2003/2004", "2004/2005", "2011/2012"), lty=c(1,2,3,4,5), col=c(1:4), horiz=FALSE)

lon.pts <- seq(170.4,170.4, by=1)
lat.pts <- rep(-46.1,length(lon.pts))

lon.pts <- 170.6
lat.pts <- -46.05
extract.pts <- cbind(lon.pts,lat.pts)
framepts <- (as.data.frame(extract.pts))


gplot(chl.b[[12]])+geom_raster(aes(fill=value), interpolate = FALSE)+
  borders(fill="black",colour="black",size=2) +
  scale_fill_distiller(palette="YlGnBu",na.value="black")+ coord_quickmap(165:174.95, -44:-48, expand=FALSE)+xlim(165,175)+ylim(-48,-44)+
  xlab("Longitude (degrees)") + ylab("Latitude (degrees)") + #theme_bw()+
  labs(fill="Chl. conc.") + theme(text = element_text(size=18)) +
  geom_point(data = framepts, aes(x=lon.pts,y=lat.pts),pch=1,col="red", size = 3,stroke = 2)



#r.mean <- calc(r.mean, function(x) {exp(x)})
extract.pts <- cbind(lon.pts,lat.pts)
ann.extract <- extract(r.mean,extract.pts,method="bilinear")

x1 <- ann.extract[1:180]
mortality <- read.csv(file="phi.csv", header=FALSE, sep=";")

years = seq(as.Date("1998-01-01"), as.Date("2012-12-31"), by="months")
years_mort <- seq(as.Date("1982-06-01"), as.Date("2012-06-1"), by="years")

df2 <- data.frame(years_mort[16:31], mortality$V2[16:31])

df2$mortality.V2 <- int(df2$mortality.V2)

plot(df2$years_mort, df2$mortality.V2, type="l")

#ymd("19980101")
dates <- interval(ymd("19980101"), ymd("20161231"))
years
matplot(years,x1,type="l",xlab="month",ylab="Chl concentration")
df <- data.frame(years, x1)

matplot(mortality$X1982.603305785124, mortality$X0.0691176470588234, type="l")
ggplot( data = df,  aes( years, x1 )) + geom_line() 

ggplot() + 
  geom_line(data=df, aes(x=years, y=x1), color='green') + 
  geom_line(data=df2, aes(x=years_mort, y=mortality$V2), color='red') 
  #scale_y_continuous(sec.axis = sec_axis(years_mort ~ df$mortality.V2, -1, name = "Relative humidity [%]"))


#tot <- lapply(tot, function(x){exp(x)})
#sum(r.mean[[1]][], na.rm=TRUE) / 48

x1 <- years
x2 <- df2$years_mort
y1 <- tot
y2 <- df2$mortality.V2.16.31.
par(mar=c(5,4,4,5)+.1)
plot(x1,y1,type="l",col="red")
par(new=TRUE)
plot(x2, y2,type="l",col="blue",xaxt="n",yaxt="n",xlab="",ylab="")
axis(4)
mtext("y2",side=4,line=3)
legend("topleft",col=c("red","blue"),lty=1,legend=c("y1","y2"))
y1

