rm(list=ls())
library(pals)
library(scales)
library(oceancolouR)
options(scipen=5)
hplc = read.csv("./Data/PhytoplanktonGrouping.csv")
phyto = read.csv("./Data/Absorption_Phytoplankton.csv")
plot.dat = merge(y=phyto , x = hplc, by.y="SAMPLE_ID", by.x="SAMPLE_ID")
in.col = alpha(cubicl(4),0.8)
in.col.plot = ifelse(plot.dat$regime=="Basin" ,in.col[2],
                     ifelse(plot.dat$regime=="Shelf" ,in.col[3],
                            ifelse(plot.dat$regime=="HighPhaeo" ,in.col[1],in.col[4])))
in.col.lty = ifelse(plot.dat$regime=="Basin" ,2,
                     ifelse(plot.dat$regime=="Shelf" ,1,
                            ifelse(plot.dat$regime=="HighPhaeo" ,2,1)))


png("./Figures/03_PhytoAbsSpectrum.png",width=6.5,height=4, units = "in",res=300,pointsize = 10 )
#Summarize the regime
par(mar=c(1.5,3,1,1),mgp=c(2,1,0),family="sans",mfrow=c(2,2),oma=c(2,0.5,0,0),xpd=NA)
##
plot(NA,ylim=c(0,0.12),xlim = c(400,700), ylab=expression("a "['PHY']* " (443) (m"^-1*")"),xlab="",xaxs="i",yaxs="i")
#high
for ( i in c("Basin","MixPhaeo","HighPhaeo","Shelf" )){
    cd = ifelse(i=="Basin" ,in.col[2],
                ifelse(i=="Shelf" ,in.col[3],
                       ifelse(i=="HighPhaeo" ,in.col[1],in.col[4])))
    in.dat = plot.dat[plot.dat$regime==i,paste0("wv",400:700,"nm")]
    sum.dat= boxplot(in.dat,plot=F)
    polygon(y=c(sum.dat$stats[2,], rev(sum.dat$stats[4,])),x=c(400:700,700:400),
           col=alpha(cd,0.25), border=alpha(cd,0.25))
    a = apply(in.dat,2, mean)
    lines(y=a,x=400:700,col=cd,lwd=1)#2,lty=2)
    rm(in.dat,cd,a,sum.dat)
}
legend("topright",legend= c("LCB","MPB","DDS","HPB"),fill=in.col[c(2,4,3,1)],ncol=2)
legend("bottomleft", legend="(a)",bty="n")
#dev.off()
##
#
wavelength = "wv443nm"#what will normalize on in future
a = plot.dat[,which(names(plot.dat)==wavelength)]
test = plot.dat
plot.dat.2 = test
for (i in 1:length(a)){test[i,paste0("wv",400:700,"nm")] = test[i,paste0("wv",400:700,"nm")] / a[i]}
plot.dat.2[,paste0("wv",400:700,"nm")] = test[,paste0("wv",400:700,"nm")]
rm(a,i,test)
tab = plot.dat[plot.dat$phaeo>=0.95,]
in.dat = tab[tab$regime=="HighPhaeo",paste0("wv",400:700,"nm")]
a = apply(in.dat,2, mean)
lines(y=a,x=400:700,col=in.col[1],lwd=2,lty=2)#2,lty=2)
tab = plot.dat[plot.dat$diatom>=0.95,]
in.dat = tab[tab$regime=="Shelf",paste0("wv",400:700,"nm")]
a = apply(in.dat,2, mean)
lines(y=a,x=400:700,col=in.col[3],lwd=2,lty=2)#2,lty=2)
##
plot(NA,ylim=c(0,1.05),xlim = c(400,700), ylab=expression("Normalized to 443 nm a"[' PHY']*"" ),xlab="",xaxs="i",yaxs="i")
polygon(y=c(0,0,1.05,1.05),x = c(460,470,470,460),col="cornsilk",border=NA,xpd=F)
polygon(y=c(0,0,1.05,1.05),x = c(470,518,518,470),col="grey85",border=NA,xpd=F)
polygon(y=c(0,0,1.05,1.05),x = c(580,600,600,580),col="cornsilk3",border=NA,xpd=F)
polygon(y=c(0,0,1.05,1.05),x = c(518,550,550,518),col="cornsilk2",border=NA,xpd=F)
#high
for ( i in c("Basin","MixPhaeo","HighPhaeo","Shelf" )){
    cd = ifelse(i=="Basin" ,in.col[2],
                ifelse(i=="Shelf" ,in.col[3],
                       ifelse(i=="HighPhaeo" ,in.col[1],in.col[4])))
    in.dat = plot.dat.2[plot.dat.2$regime==i,paste0("wv",400:700,"nm")]
    a = apply(in.dat,2, mean)
    lines(y=a,x=400:700,col=cd,lwd=1)
    rm(cd,in.dat,a)
}

legend("bottomleft", legend="(b)",bty="n")
rm(plot.dat.2,i)
box()
##
#
wavelength = "wv675nm"#what will normalize on in future
a = plot.dat[,which(names(plot.dat)==wavelength)]
test = plot.dat
plot.dat.2 = test
for (i in 1:length(a)){test[i,paste0("wv",400:700,"nm")] = test[i,paste0("wv",400:700,"nm")] / a[i]}
plot.dat.2[,paste0("wv",400:700,"nm")] = test[,paste0("wv",400:700,"nm")]
rm(a,i,test)
plot(NA,ylim=c(0,3.5),xlim = c(400,700), ylab=expression("Normalized to 675 nm a "['PHY']*"" ),xlab="Wavelength (nm)",xaxs="i",yaxs="i")
polygon(y=c(0,0,3.55,3.55),x = c(460,470,470,460),col="cornsilk",border=NA,xpd=F)
polygon(y=c(0,0,3.55,3.55),x = c(470,518,518,470),col="grey85",border=NA,xpd=F)
polygon(y=c(0,0,3.55,3.55),x = c(580,600,600,580),col="cornsilk3",border=NA,xpd=F)
polygon(y=c(0,0,3.55,3.55),x = c(518,550,550,518),col="cornsilk2",border=NA,xpd=F)
for ( i in c("Basin","MixPhaeo","HighPhaeo","Shelf" )){
    cd = ifelse(i=="Basin" ,in.col[2],
                ifelse(i=="Shelf" ,in.col[3],
                       ifelse(i=="HighPhaeo" ,in.col[1],in.col[4])))
    in.dat = plot.dat.2[plot.dat.2$regime==i,paste0("wv",400:700,"nm")]
    a = apply(in.dat,2, mean)
    lines(y=a,x=400:700,col=cd,lwd=1)
    rm(cd,in.dat,a)
}
legend("bottomleft", legend="(c)",bty="n")
rm(plot.dat.2,i)
box()
#
#Normalize
a = plot.dat$chla
test = plot.dat
plot.dat.2 = test
for (i in 1:length(a)){test[i,paste0("wv",400:700,"nm")] = test[i,paste0("wv",400:700,"nm")] / a[i]}
plot.dat.2[,paste0("wv",400:700,"nm")] = test[,paste0("wv",400:700,"nm")]
rm(a,i,test)
#
plot(NA,ylim=c(0,0.065),xlim = c(400,700), ylab=expression("a*"['PHY']* " (m"^2*"/ mg chl)"),
     xlab="Wavelength (nm)",xaxs="i",yaxs="i")
polygon(y=c(0,0,1.05,1.05),x = c(460,470,470,460),col="cornsilk",border=NA,xpd=F)
polygon(y=c(0,0,1.05,1.05),x = c(470,518,518,470),col="grey85",border=NA,xpd=F)
polygon(y=c(0,0,1.05,1.05),x = c(580,600,600,580),col="cornsilk3",border=NA,xpd=F)
polygon(y=c(0,0,1.05,1.05),x = c(518,550,550,518),col="cornsilk2",border=NA,xpd=F)
#high
for ( i in c("Basin","MixPhaeo","HighPhaeo","Shelf" )){
    cd = ifelse(i=="Basin" ,in.col[2],
                ifelse(i=="Shelf" ,in.col[3],
                       ifelse(i=="HighPhaeo" ,in.col[1],in.col[4])))
    in.dat = plot.dat.2[plot.dat.2$regime==i,paste0("wv",400:700,"nm")]
    a = apply(in.dat,2, mean)
    lines(y=a,x=400:700,col=cd,lwd=1)
    rm(cd,in.dat,a)
}
legend("bottomleft", legend="(d)",bty="n")
rm(plot.dat.2,i)
box()
dev.off()