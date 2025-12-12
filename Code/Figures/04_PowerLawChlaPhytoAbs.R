library(pals)
library(scales)
library(oceancolouR)
options(scipen=5)
hplc = read.csv("./Data/PhytoplanktonGrouping.csv")
hplc = hplc[,c("SAMPLE_ID"  , "chla", "regime")]
phyto = read.csv("./Data/Absorption_Phytoplankton.csv")
phyto = phyto[,c("SAMPLE_ID","wv443nm")]
plot.dat = merge(y=phyto , x = hplc, by.y="SAMPLE_ID", by.x="SAMPLE_ID")
##
rgrlog10 = function(x,y){
    y = log10(y)
    x = log10(x)
    test = lmodel2::lmodel2(y ~ x)
    tdf = do.call(dplyr::bind_cols, oceancolouR::get_lm_stats(test,method="SMA")) 
    tdf[1:3] = round(tdf[1:3], 4)
    RMSE = round(rmse(x =x, y=y  ),4)
    tdf = cbind(tdf,RMSE)
    return(tdf)
}
##
#
in.col = alpha(cubicl(4),0.8)
in.col.plot = ifelse(plot.dat$regime=="Basin" ,in.col[2],
                ifelse(plot.dat$regime=="Shelf" ,in.col[3],
                       ifelse(plot.dat$regime=="HighPhaeo" ,in.col[1],in.col[4])))
png("./Figures/04_PowerLawChlaPhytoAbs.png",width=6.5,height=4.5, units = "in",res=300,pointsize = 10 )
par(mar=c(3,3.5,1,1),mgp=c(2,1,0),family="serif")
plot(x = plot.dat$chla, y = plot.dat$wv443 ,
     pch = 21,bg=in.col.plot, col="grey30",log = "xy",
     ylab=expression("a"['PHY']* " (443) (m"^-1*")"),
     xlab = expression("chl-a (mg m"^-3*")"))
legend("topleft", legend=c( "LCB" , "MPB","HPB" ,"DDS"), col = "grey30",ncol=2,
       pt.bg=in.col[c(2,4,1,3)],pch=21)
abline(b =0.616, a = log10(0.03801894), untf=F,col="grey65",lty=4,lwd=2)#Devred lab sea
abline(b = 0.733, a = log10(0.0315), untf=F,col="grey55",lty=3,lwd=2)#Matsuoka et al. (2014) Fig 2
abline(b = 0.728, a = log10(0.0654), untf=F,col="grey25",lty=6,lwd=2)#Bricaud 2004
legend("bottomright", legend=c(expression("a"['PHY']* " (443) = 0.038 x chl-a "^-0.616*" (Devred et al., 2022)"),
                               expression("a"['PHY']* " (443) = 0.032 x chl-a "^-0.733*" (Matsuoka et at., 2014)"), 
                               expression("a"['PHY']* " (443) = 0.065 x chl-a "^-0.728*" (Bricaud et al., 2004)")),
       col = c("grey65", "grey55", "grey25"),lty=c(4,3,6),lwd=2)
#Basin slope
slog = rgrlog10(x = plot.dat$chla[plot.dat$regime=="Basin"] , 
                y= plot.dat$wv443[plot.dat$regime=="Basin"]  )
slog = round(slog,3)
abline(b =slog$Slope, a=slog$Intercept, untf=F,col=in.col[2],lty=2,lwd=2)
basin=slog
#
#Shelf slope
slog = rgrlog10(x = plot.dat$chla[plot.dat$regime=="Shelf"] , 
                y= plot.dat$wv443[plot.dat$regime=="Shelf"]  )
slog = round(slog,3)
abline(b =slog$Slope, a=slog$Intercept, untf=F,col=in.col[3],lty=2,lwd=2)
shelf=slog
#
#Mix slope
slog = rgrlog10(x = plot.dat$chla[plot.dat$regime=="MixPhaeo"] , 
                y= plot.dat$wv443[plot.dat$regime=="MixPhaeo"]  )
slog = round(slog,3)
abline(b =slog$Slope, a=slog$Intercept, untf=F,col=in.col[4],lty=2,lwd=2)
mix=slog
#
#phaeo slope
x1 = plot.dat$chla[plot.dat$regime=="HighPhaeo"]
y1 = plot.dat$wv443[plot.dat$regime=="HighPhaeo"]
slog = rgrlog10(x = x1, 
                y=  y1  )
slog = round(slog,3)
abline(b =slog$Slope, a=slog$Intercept, untf=F,col=in.col[1],lty=2,lwd=2)
high=slog
dev.off()
