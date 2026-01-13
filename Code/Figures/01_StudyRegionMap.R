rm(list=ls())
library(terra)
library(pals)
library(scales)
library(marmap)
library(sf)
options(scipen=5)
plot.dat = read.csv("./Data/PhytoplanktonGrouping.csv")
in.col = alpha(cubicl(4),0.8)
plot.dat$in.col.plot = ifelse(plot.dat$regime=="Basin" ,in.col[2],
                     ifelse(plot.dat$regime=="Shelf" ,in.col[3],
                            ifelse(plot.dat$regime=="HighPhaeo" ,in.col[1],in.col[4])))
labsea = vect("./Data/CoastLine/LabSea.shp")
vect.points = data.frame(Lat = tapply(plot.dat$STARTLAT, plot.dat$STATION, "mean"),
                         Lon = tapply(plot.dat$STARTLON, plot.dat$STATION, "mean"),
                         LCB = table(plot.dat$STATION, plot.dat$regime)[,1],
                         HPB = table(plot.dat$STATION, plot.dat$regime)[,2],
                         MPB = table(plot.dat$STATION, plot.dat$regime)[,3],
                         DDS = table(plot.dat$STATION, plot.dat$regime)[,4])
vect.points$STATION = row.names(vect.points)                         
vect.points = vect(vect.points, geom=c("Lon", "Lat" ),crs=crs(labsea))
vect.points = sf::st_as_sf(vect.points)
test = sf::st_make_grid(vect.points, c(0.75, 0.75), what = "polygons", square = F)
test = vect(test)
vect.points = vect(vect.points)
##
# Find which polygons contain which points (returns indices)
a = extract(test,vect.points)[,2]
#Number of regime points per polygon
dat = cbind(a,as.data.frame(vect.points)[,1:4])
col.dat = data.frame( LCB = tapply(dat$LCB,dat$a,"sum"),
                      HPB = tapply(dat$HPB,dat$a,"sum"),
                      MPB = tapply(dat$MPB,dat$a,"sum"),
                      DDS = tapply(dat$DDS,dat$a,"sum"))
#
col.dat$sum = apply(col.dat,1,sum)
col.dat$LCB.p =  round(col.dat$LCB/col.dat$sum, 3)
col.dat$DDS.p =  round(col.dat$DDS/col.dat$sum, 3)
col.dat$HPB.p =  round(col.dat$HPB/col.dat$sum, 3)
col.dat$MPB.p =  round(col.dat$MPB/col.dat$sum, 3)
##
col.dat$LCB.col = cubicl(4)[2]
col.dat$LCB.tran = alpha(col.dat$LCB.col,col.dat$LCB.p )
#
col.dat$DDS.col = cubicl(4)[3]
col.dat$DDS.tran = alpha(col.dat$DDS.col,col.dat$DDS.p)
#
col.dat$HPB.col = cubicl(4)[1]
col.dat$HPB.tran = alpha(col.dat$HPB.col,col.dat$HPB.p)
#
col.dat$MPB.col = cubicl(4)[4]
col.dat$MPB.tran = alpha(col.dat$MPB.col,col.dat$MPB.p)
#
#
b = test[unique(a)]
#
# map boundaries
xlim = c(-65.5,-42.5)
ylim = c(51,62)
bathy = getNOAA.bathy(lon1 = xlim[1], lon2 = xlim[2], lat1 = ylim[1], lat2 = ylim[2], resolution = 1)
##
rm(plot.dat,test,vect.points,a,in.col,xlim,ylim,dat)
##
png("./Figures/01_StudyAreaMAp.png",width=6.2,height=5, units = "in",res=300,pointsize = 10 )
par(mgp=c(2,1,0),family="serif",mfrow=c(2,2))
plot(labsea,buffer=F,col="grey90",ylim=c(51,62),xlim=c(-65.5,-43),mar=c(2.5,2,0.5,3.6),
     ylab="Longitude (°W)", xlab="Latitude (°N)",cex=1.2)
plot(bathy,step=1000, deep=-3000,shallow=-1000,drawlabels=T,n=3,add=T)
plot(b,add=T, col = col.dat$DDS.tran)
legend("bottomleft","(a)",bty="n")
legend("bottomright","DDS   ",bty="n")
in.col = rep(cubicl(4)[3],21)
in.alpha = seq(0,1,0.05)
in.col = alpha(in.col,in.alpha)
shape::colorlegend(posx = c(0.86, 0.895), left = F, col =in.col,main="N (%)",
                   zlim = c(0, 100), digit = 0, dz = 20,xpd=NA) 
plot(labsea,buffer=F,col="grey90",ylim=c(51,62),xlim=c(-65.5,-43),mar=c(2.5,2,0.5,3.6),
     ylab="Longitude (°W)", xlab="Latitude (°N)",cex=1.2)
plot(bathy,step=1000, deep=-3000,shallow=-1000,drawlabels=T,n=3,add=T)
plot(b,add=T, col = col.dat$LCB.tran)
legend("bottomleft","(b)",bty="n")
legend("bottomright","LCB   ",bty="n")
in.col = rep(cubicl(4)[2],21)
in.alpha = seq(0,1,0.05)
in.col = alpha(in.col,in.alpha)
shape::colorlegend(posx = c(0.86, 0.895), left = F, col =in.col, main="N (%)",
                   zlim = c(0, 100), digit = 0, dz = 20,xpd=NA) 
plot(labsea,buffer=F,col="grey90",ylim=c(51,62),xlim=c(-65.5,-43),mar=c(2.5,2,0.5,3.6),
     ylab="Longitude (°W)", xlab="Latitude (°N)",cex=1.2)
plot(bathy,step=1000, deep=-3000,shallow=-1000,drawlabels=T,n=3,add=T)
plot(b,add=T, col = col.dat$HPB.tran)
legend("bottomleft","(c)",bty="n")
legend("bottomright","HPB   ",bty="n")
in.col = rep(cubicl(4)[1],21)
in.alpha = seq(0,1,0.05)
in.col = alpha(in.col,in.alpha)
shape::colorlegend(posx =c(0.86, 0.895), left = F, col =in.col,main="N (%)",
                   zlim = c(0, 100), digit = 0, dz = 20,xpd=NA) 
plot(labsea,buffer=F,col="grey90",ylim=c(51,62),xlim=c(-65.5,-43),mar=c(2.5,2,0.5,3.6),
     ylab="Longitude (°W)", xlab="Latitude (°N)",cex=1.2)
plot(bathy,step=1000, deep=-3000,shallow=-1000,drawlabels=T, n=3,add=T)
plot(b,add=T, col = col.dat$MPB.tran)
legend("bottomleft","(d)",bty="n")
legend("bottomright","MPB   ",bty="n")
in.col = rep(cubicl(4)[4],21)
in.alpha = seq(0,1,0.05)
in.col = alpha(in.col,in.alpha)
shape::colorlegend(posx = c(0.86, 0.895), left = F, col =in.col,main="N (%)",
                   zlim = c(0, 100), digit = 0, dz = 20,xpd=NA) 
dev.off()
rm(b,col.dat,labsea,bathy,in.alpha,in.col)
