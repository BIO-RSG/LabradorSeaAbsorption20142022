library(pals)
library(scales)
hplc = read.csv("./Data/PhytoplanktonGrouping.csv")
phyto = read.csv("./Data/Absorption_Phytoplankton.csv")
phyto = phyto[,c("SAMPLE_ID","wv443nm")]
names(phyto) = c("SAMPLE_ID","aphy")
hplc = merge(y=phyto , x = hplc, by.y="SAMPLE_ID", by.x="SAMPLE_ID")
phyto = read.csv("./Data/Absorption_Detritus.csv")
phyto = phyto[,c("SAMPLE_ID","wv443nm")]
names(phyto) = c("SAMPLE_ID","anap")
hplc = merge(y=phyto , x = hplc, by.y="SAMPLE_ID", by.x="SAMPLE_ID",all.x=T)
phyto = read.csv("./Data/Absorption_CDOM.csv")
phyto = phyto[,c("SAMPLE_ID", "slope350400","wv443nm")]
names(phyto) = c("SAMPLE_ID", "slope350400","acdom")
hplc = merge(y=phyto , x = hplc, by.y="SAMPLE_ID", by.x="SAMPLE_ID",all.x=T)
rm(phyto)
##
in.col = alpha(cubicl(4),0.5)
hplc$diatom = hplc$diatom*100
hplc$phaeo = hplc$phaeo*100
#
png(paste0("./Figures/SI1-Chla-diatom-phaeo.png"),
    width=6.5,height=5.5, units = "in",res=300,pointsize = 10 )
par(mfrow=c(3,2),mar=c(3,3.5,1,1),mgp=c(2.5,1,0),xpd=NA,oma=c(1,1,0,0))
boxplot(hplc$chla~hplc$regime,ylab= expression("chl-a (mg m"^-3*")"),
        xlab="",col=in.col[c(2,1,4,3)],xaxt="n",xpd=NA)
legend("topright","(a)",bty="n")
axis(1, at=1:4, labels=c("LCB","HPB","MPB","DDS"))
text(x=(1:4)-0.5,y=rep(12,4), labels = c("D","AB","AC","A"))
plot(NA,xaxs="i",yaxs="i",ylab="Density",
     ylim=c(0,2),main="",xlim=c(0,max(hplc$chla)*1.1),
     xlab=expression("chl-a (mg m"^-3*")") )
polygon(density(hplc$chla[hplc$regime=="Basin"]),col=in.col[2],xpd=F)
polygon(density(hplc$chla[hplc$regime=="HighPhaeo"]),col=in.col[1],xpd=F)
polygon(density(hplc$chla[hplc$regime=="MixPhaeo"]),col=in.col[4],xpd=F)
polygon(density(hplc$chla[hplc$regime=="Shelf"]),col=in.col[3],xpd=F)
legend("topright","(b)",bty="n")
##
boxplot(hplc$diatom~hplc$regime,ylab= "Diatom (%)",
        xlab="",col=in.col[c(2,1,4,3)],xaxt="n")
legend("topright","(c)",bty="n")
axis(1, at=1:4, labels=c("LCB","HPB","MPB","DDS"))
text(x=(1:4)-0.5,y=rep(100,4), labels = c("B","B","B","A"))
plot(NA,xaxs="i",yaxs="i",ylab="Density",
     ylim=c(0,0.03),main="",xlim=c(0,max(hplc$diatom)*1.1),
     xlab="Diatom (%)" )
polygon(density(hplc$diatom[hplc$regime=="Basin"]),col=in.col[2],xpd=F)
polygon(density(hplc$diatom[hplc$regime=="HighPhaeo"]),col=in.col[1],xpd=F)
polygon(density(hplc$diatom[hplc$regime=="MixPhaeo"]),col=in.col[4],xpd=F)
polygon(density(hplc$diatom[hplc$regime=="Shelf"]),col=in.col[3],xpd=F)
legend("topright","(d)",bty="n") 
#
boxplot(hplc$phaeo~hplc$regime,ylab= expression(italic("Phaeocystis")~" (%)"),
        xlab="",col=in.col[c(2,1,4,3)],xaxt="n")
legend("topright","(e)",bty="n")
axis(1, at=1:4, labels=c("LCB","HPB","MPB","DDS"))
text(x=(1:4)-0.5,y=rep(100,4), labels = c("A","B","C","A"))
plot(NA,xaxs="i",yaxs="i",ylab="Density",
     ylim=c(0,0.1),main="",xlim=c(0,max(hplc$phaeo)*1.1),
     xlab=expression(italic("Phaeocystis")~" (%)") )
polygon(density(hplc$phaeo[hplc$regime=="Basin"]),col=in.col[2],xpd=F)
polygon(density(hplc$phaeo[hplc$regime=="HighPhaeo"]),col=in.col[1],xpd=F)
polygon(density(hplc$phaeo[hplc$regime=="MixPhaeo"]),col=in.col[4],xpd=F)
polygon(density(hplc$phaeo[hplc$regime=="Shelf"]),col=in.col[3],xpd=F)
legend("topright","(f)",bty="n") 
##
dev.off()
##
