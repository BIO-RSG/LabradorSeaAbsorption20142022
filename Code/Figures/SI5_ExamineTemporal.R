library(pals)
library(scales)
library(oceancolouR)
library(Ternary)
options(scipen=5)

##
wavelength = "wv443nm"
hplc = read.csv("./Data/PhytoplanktonGrouping.csv")
phyto = read.csv("./Data/Absorption_Phytoplankton.csv")
phyto = phyto[,c("SAMPLE_ID",wavelength)]
names(phyto) = c("SAMPLE_ID","phyto")
hplc = merge(y=phyto , x = hplc, by.y="SAMPLE_ID", by.x="SAMPLE_ID")
phyto = read.csv("./Data/Absorption_Detritus.csv")
phyto = phyto[,c("SAMPLE_ID",wavelength)]
names(phyto) = c("SAMPLE_ID","det")
hplc = merge(y=phyto , x = hplc, by.y="SAMPLE_ID", by.x="SAMPLE_ID",all.x=T)
phyto = read.csv("./Data/Absorption_CDOM.csv")
phyto = phyto[,c("SAMPLE_ID", wavelength)]
names(phyto) = c("SAMPLE_ID", "cdom")
hplc = merge(y=phyto , x = hplc, by.y="SAMPLE_ID", by.x="SAMPLE_ID",all.x=T)
plot.dat = hplc
rm(phyto,hplc)
in.col = alpha(cubicl(4),0.8)
plot.dat$YEAR = factor(plot.dat$YEAR, levels= unique(plot.dat$YEAR))
plot.dat$regime = factor(plot.dat$regime, levels= c("Basin","Shelf","MixPhaeo", "HighPhaeo"))
plot.dat$in.col.plot = ifelse(plot.dat$regime=="Basin" ,in.col[2],
                         ifelse(plot.dat$regime=="Shelf" ,in.col[3],
                            ifelse(plot.dat$regime=="HighPhaeo" ,in.col[1],in.col[4])))
png("./Figures/SI5_TemporalAbs.png",width=6.5,height=6.5, units = "in",res=300,pointsize = 10 )
par(mar=c(2,3.5,1,1),mgp=c(2,1,0),mfrow=c(3,1),oma=c(1,0,0,0),xpd=NA,family="serif")
plot(plot.dat$phyto ~ plot.dat$YEAR,
     ylim = c(0.003,0.21),log="y",
     ylab = expression("a"['PHY']* " (443) (m"^-1*")"),
     xlab="")
abline(h=c(0.085, 0.022, 0.046, 0.039),col=cubicl(4),lty=2,lwd=2,xpd=F)
legend("bottomleft","(a)",bty="n")
plot(plot.dat$det ~ plot.dat$YEAR,
     ylim = c(0.0002,0.03),log="y",
     ylab = expression("a"['NAP']* " (443) (m"^-1*")"),
     xlab="")
abline(h=c(0.01, 0.003, 0.007, 0.005),col=cubicl(4),lty=2,lwd=2,xpd=F)
legend("bottomleft","(b)",bty="n")
plot(plot.dat$cdom ~ plot.dat$YEAR,
     ylim = c(0.004,0.2),log="y",
     ylab = expression("a"['CDOM']* " (443) (m"^-1*")"),
     xlab="Year",xpd=NA)
abline(h=c(0.051, 0.038, 0.085, 0.049),col=cubicl(4),lty=2,lwd=2,xpd=F)
legend("bottomleft","(c)",bty="n")
legend("bottom", legend=c( "LCB" , "MPB","HPB" ,"DDS"),lty=2,lwd=2,title="Regime Median",
       col=in.col[c(2,4,1,3)],horiz=T,bty="n")
dev.off()
###
