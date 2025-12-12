library(pals)
library(scales)
options(scipen=5)

hplc = read.csv("./Data/PhytoplanktonGrouping.csv")
##
a = sort(hplc$phaeo, decreasing = F,index.return=T)$ix#sort on phaeo to highlight high values
hplc = hplc[a,]
rm(a)
##
#
##Define colour based on relative phaeo contribution
#phaeo.col.factor =  cut(hplc$phaeo, breaks=seq(0.0,1,0.05), include.lowest = T)
#in.col = alpha(cubicl(20),0.8)
#phaeo.col = in.col[phaeo.col.factor]
phaeo.col.factor =  cut(hplc$phaeo, breaks=seq(0.0,1,0.2), include.lowest = T)
in.col = alpha(cubicl(5),0.8)
phaeo.col = in.col[phaeo.col.factor]

#
png("./Figures/01_TempSal.png",width=6.5,height=5, units = "in",res=300,pointsize = 10 )
par(mar=c(1,3,1,7),mgp=c(2,1,0),family="serif",mfrow=c(2,1),oma=c(2,0,0,0),xpd=NA)
plot(x = hplc$CTD_Temp, y = hplc$CTD_Salinity,
     xlab = "", ylab="Salinity (PSU)",cex=1.2,
     pch=21, bg=phaeo.col,col="grey30")
legend(x=-2,y=35,legend="(a)",bty="n")
#shape::colorlegend(posx = c(0.89, 0.93), left = F, col =in.col, 
 #                  main=expression(italic("Phaeocystis")~" (%)"),
  #                 zlim = c(0, 100), digit = 0, dz = 10,xpd=NA)  
shape::colorlegend(posx = c(0.89, 0.93), left = F, col =in.col, 
                  main=expression(italic("Phaeocystis")~" (%)"),
                zlim = c(0, 100), digit = 0, dz = 20,xpd=NA)  

hplc = read.csv("./Data/PhytoplanktonGrouping.csv")
a = sort(hplc$diatom, decreasing = F,index.return=T)$ix#sort on diatom to highlight high values
hplc = hplc[a,]
rm(a)
diatom.col.factor =  cut(hplc$diatom, breaks=seq(0.0,1,0.2), include.lowest = T)
in.col = alpha(cubicl(5),0.8)
diatom.col = in.col[diatom.col.factor]
plot(x = hplc$CTD_Temp, y = hplc$CTD_Salinity,cex=1.2,
     xlab = "Temperature (°C)", ylab="Salinity (PSU)",
     pch=21, bg=diatom.col,col="grey30")
legend(x=-2,y=35,legend="(b)",bty="n")
shape::colorlegend(posx = c(0.89, 0.93), left = F, col =in.col, 
                   main="Diatom (%)",
                   zlim = c(0, 100), digit = 0, dz = 20,xpd=NA) 
dev.off()



