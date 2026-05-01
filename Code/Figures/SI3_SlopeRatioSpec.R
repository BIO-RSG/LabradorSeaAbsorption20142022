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
png(paste0("./Figures/SI3-Slope-Ratio-Spec.png"),
    width=6.5,height=5.5, units = "in",res=300,pointsize = 10 )
par(mfrow=c(3,2),mar=c(3,3.5,1,1),mgp=c(2.5,1,0),xpd=NA,oma=c(1,1,0,0))
boxplot(hplc$slope350400~hplc$regime,ylab= expression("S"['CDOM']* " (350-400) (nm"^-1*")"),
        xlab="",col=in.col[c(2,1,4,3)],xaxt="n",xpd=NA)
legend("topright","(a)",bty="n")
axis(1, at=1:4, labels=c("LCB","HPB","MPB","DDS"))
text(x=(1:4)-0.25,y=rep(0.0185,4), labels = c("A","A","A","A"))
plot(NA,xaxs="i",yaxs="i",ylab="Density",
     ylim=c(0,600),main="",xlim=c(0,max(hplc$slope350400,na.rm=T)*1.1),
     xlab=expression("S"['CDOM']* " (350-400) (nm"^-1*")") )
polygon(density(hplc$slope350400[hplc$regime=="Basin"],na.rm=T),col=in.col[2],xpd=F)
polygon(density(hplc$slope350400[hplc$regime=="HighPhaeo"],na.rm=T),col=in.col[1],xpd=F)
polygon(density(hplc$slope350400[hplc$regime=="MixPhaeo"],na.rm=T),col=in.col[4],xpd=F)
polygon(density(hplc$slope350400[hplc$regime=="Shelf"],na.rm=T),col=in.col[3],xpd=F)
legend("topright","(b)",bty="n")
##
dat = hplc$acdom/hplc$aphy
boxplot(dat~hplc$regime,ylab= expression("a"['CDOM']* " (443) : a"['PHY']*" (443)"),
        xlab="",col=in.col[c(2,1,4,3)],xaxt="n")
legend("topright","(c)",bty="n")
axis(1, at=1:4, labels=c("LCB","HPB","MPB","DDS"))
text(x=(1:4)-0.25,y=rep(5,4), labels = c("AB","B","A","A"))
plot(NA,xaxs="i",yaxs="i",ylab="Density",
     ylim=c(0,2),main="",xlim=c(0,max(dat,na.rm=T)*1.1),
     xlab=expression("a"['CDOM']* " (443) : a"['PHY']*" (443)"))
polygon(density(dat[hplc$regime=="Basin"],na.rm=T),col=in.col[2],xpd=F)
polygon(density(dat[hplc$regime=="HighPhaeo"],na.rm=T),col=in.col[1],xpd=F)
polygon(density(dat[hplc$regime=="MixPhaeo"],na.rm=T),col=in.col[4],xpd=F)
polygon(density(dat[hplc$regime=="Shelf"],na.rm=T),col=in.col[3],xpd=F)
legend("topright","(d)",bty="n") 
#
dat = hplc$aphy/hplc$chla
boxplot(dat~hplc$regime,ylab= expression("a*"['PHY']* " (m" ^2*" (mg chl-a)" ^-1*")"),
        xlab="",col=in.col[c(2,1,4,3)],xaxt="n")
legend("topright","(e)",bty="n")
axis(1, at=1:4, labels=c("LCB","HPB","MPB","DDS"))
text(x=(1:4)-0.25,y=rep(0.15,4), labels = c("D","B","C","A"))
plot(NA,xaxs="i",yaxs="i",ylab="Density",
     ylim=c(0,60),main="",xlim=c(0,max(dat,na.rm=T)*1.1),
     xlab=expression("a*"['PHY']* " (m" ^2*" (mg chl-a)" ^-1*")"))
polygon(density(dat[hplc$regime=="Basin"],na.rm=T),col=in.col[2],xpd=F)
polygon(density(dat[hplc$regime=="HighPhaeo"],na.rm=T),col=in.col[1],xpd=F)
polygon(density(dat[hplc$regime=="MixPhaeo"],na.rm=T),col=in.col[4],xpd=F)
polygon(density(dat[hplc$regime=="Shelf"],na.rm=T),col=in.col[3],xpd=F)
legend("topright","(f)",bty="n") 
##
dev.off()
##
