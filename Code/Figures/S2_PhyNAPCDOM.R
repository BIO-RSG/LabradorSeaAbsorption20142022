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
png(paste0("./Figures/SI2-Phy-NAP-CDOM.png"),
    width=6.5,height=5.5, units = "in",res=300,pointsize = 10 )
par(mfrow=c(3,2),mar=c(3,3.5,1,1),mgp=c(2.5,1,0),xpd=NA,oma=c(1,1,0,0))
boxplot(hplc$aphy~hplc$regime,ylab= expression("a"['PHY']* " (443) (m"^-1*")"),
        xlab="",col=in.col[c(2,1,4,3)],xaxt="n",xpd=NA)
legend("topright","(a)",bty="n")
axis(1, at=1:4, labels=c("LCB","HPB","MPB","DDS"))
text(x=(1:4)-0.25,y=rep(0.15,4), labels = c("C","B","A","A"))
plot(NA,xaxs="i",yaxs="i",ylab="Density",
     ylim=c(0,40),main="",xlim=c(0,max(hplc$aphy)*1.1),
     xlab=expression("a"['PHY']* " (443) (m"^-1*")") )
polygon(density(hplc$aphy[hplc$regime=="Basin"]),col=in.col[2],xpd=F)
polygon(density(hplc$aphy[hplc$regime=="HighPhaeo"]),col=in.col[1],xpd=F)
polygon(density(hplc$aphy[hplc$regime=="MixPhaeo"]),col=in.col[4],xpd=F)
polygon(density(hplc$aphy[hplc$regime=="Shelf"]),col=in.col[3],xpd=F)
legend("topright","(b)",bty="n")
##
boxplot(hplc$anap~hplc$regime,ylab= expression("a"['NAP']* " (443) (m"^-1*")") ,
        xlab="",col=in.col[c(2,1,4,3)],xaxt="n")
legend("topright","(c)",bty="n")
axis(1, at=1:4, labels=c("LCB","HPB","MPB","DDS"))
text(x=(1:4)-0.25,y=rep(0.03,4), labels = c("B","A","B","A"))
plot(NA,xaxs="i",yaxs="i",ylab="Density",
     ylim=c(0,170),main="",xlim=c(0,max(hplc$anap,na.rm=T)*1.1),
     xlab=expression("a"['NAP']* " (443) (m"^-1*")")  )
polygon(density(hplc$anap[hplc$regime=="Basin"],na.rm=T),col=in.col[2],xpd=F)
polygon(density(hplc$anap[hplc$regime=="HighPhaeo"],na.rm=T),col=in.col[1],xpd=F)
polygon(density(hplc$anap[hplc$regime=="MixPhaeo"],na.rm=T),col=in.col[4],xpd=F)
polygon(density(hplc$anap[hplc$regime=="Shelf"],na.rm=T),col=in.col[3],xpd=F)
legend("topright","(d)",bty="n") 
#
boxplot(hplc$acdom~hplc$regime,ylab= expression("a"['CDOM']* " (443) (m"^-1*")"),
        xlab="",col=in.col[c(2,1,4,3)],xaxt="n")
legend("topright","(e)",bty="n")
axis(1, at=1:4, labels=c("LCB","HPB","MPB","DDS"))
text(x=(1:4)-0.25,y=rep(0.15,4), labels = c("AB","B","B","A"))
plot(NA,xaxs="i",yaxs="i",ylab="Density",
     ylim=c(0,50),main="",xlim=c(0,max(hplc$acdom,na.rm=T)*1.1),
     xlab=expression("a"['CDOM']* " (443) (m"^-1*")") )
polygon(density(hplc$acdom[hplc$regime=="Basin"],na.rm=T),col=in.col[2],xpd=F)
polygon(density(hplc$acdom[hplc$regime=="HighPhaeo"],na.rm=T),col=in.col[1],xpd=F)
polygon(density(hplc$acdom[hplc$regime=="MixPhaeo"],na.rm=T),col=in.col[4],xpd=F)
polygon(density(hplc$acdom[hplc$regime=="Shelf"],na.rm=T),col=in.col[3],xpd=F)
legend("topright","(f)",bty="n") 
##
dev.off()
##
