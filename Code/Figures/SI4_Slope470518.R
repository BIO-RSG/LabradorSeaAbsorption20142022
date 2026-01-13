rm(list=ls())
library(pals)
library(scales)
options(scipen=5)
hplc = read.csv("./Data/PhytoplanktonGrouping.csv")
phyto = read.csv("./Data/Absorption_Phytoplankton.csv")
plot.dat = merge(y=phyto , x = hplc, by.y="SAMPLE_ID", by.x="SAMPLE_ID")
rm(hplc,phyto)
##
in.col = alpha(cubicl(4),0.8)
in.col.plot = ifelse(plot.dat$regime=="Basin" ,in.col[2],
                     ifelse(plot.dat$regime=="Shelf" ,in.col[3],
                            ifelse(plot.dat$regime=="HighPhaeo" ,in.col[1],in.col[4])))
in.col.lty = ifelse(plot.dat$regime=="Basin" ,in.col[2],
                    ifelse(plot.dat$regime=="Shelf" ,in.col[3],
                           ifelse(plot.dat$regime=="HighPhaeo" ,in.col[1],in.col[4])))
png("./Figures/SI4_PhytoSlope470518_SI.png",
    width=6.5,height=6.5, units = "in",res=300,pointsize = 10 )
par(mar=c(1,3,1,0.75),mgp=c(2.25,1,0),mfrow=c(3,3),oma=c(2.2,0.5,0,0.75),xpd=NA)
## Row 1
plot.dat.norm = plot.dat[,paste0("wv", 400:700,"nm")]
sl = (plot.dat.norm$wv518nm - plot.dat.norm$wv471nm)/(518-471)
sl2=sl
boxplot(sl ~ as.factor(plot.dat$regime),col=in.col[c(2,1,4,3)],ylab=expression("a"['PHY']*"" ),
        ylim=c(-0.00258,-0.000043),
        xlab ="",horizontal=T,names=rev(c("DDS","MPB","HPB","LCB")))
legend("bottomleft","(a)",bty="n")
a = plot.dat$phaeo*plot.dat$chla
pch.in = ifelse(plot.dat$phaeo[plot.dat$regime !="Shelf"] <=0.25,0.75,
                ifelse(plot.dat$phaeo[plot.dat$regime !="Shelf"] > 0.25 & plot.dat$phaeo[plot.dat$regime !="Shelf"] <=0.5,1,
                       ifelse(plot.dat$phaeo[plot.dat$regime !="Shelf"] > 0.5 & plot.dat$phaeo[plot.dat$regime !="Shelf"] <=0.75,1.25,1.5)))
in.dat = data.frame(y = a[plot.dat$regime !="Shelf"] ,
                    x = sl[plot.dat$regime !="Shelf"],
                    c = pch.in,
                    col =in.col.plot[plot.dat$regime !="Shelf"])
in.dat = in.dat[in.dat$y>0,]
plot(y = in.dat$y , x=in.dat$x,ylim=c(0.01,15), 
     bg=in.dat$col,pch=21,cex=in.dat$c,xlim=c(-0.00258,-0.000043),
     col=alpha(in.dat$col,0.5), 
     ylab=expression(italic("Phaeocystis")~" (mg m"^-3*")"), 
     xlab= "",log="y")#,xaxt="n")
a = ifelse(a>0,a,NA)
a = lm( log10(a[plot.dat$regime =="HighPhaeo"]) ~sl[plot.dat$regime =="HighPhaeo"])
summary(a)
abline(a,xpd=F,col = in.col[1],lty=2,lwd=2)
text(y=12,x = -0.002,expression("R"^2*" = 0.54"), col="black",cex=1.2)
legend("bottomleft","(b)",bty="n")
legend("bottomright", legend =c("0 - 25","25 - 50", "50 - 75","75 - 100"),pch=21, bty="n",
       pt.cex=c(0.75,1,1.25,1.5), title="Biomass (%)",ncol=2)
a = plot.dat$diatom*plot.dat$chla
pch.in = ifelse(plot.dat$diatom[plot.dat$regime !="HighPhaeo"] <=0.25,0.75,
                ifelse(plot.dat$diatom[plot.dat$regime !="HighPhaeo"] > 0.25 & plot.dat$diatom[plot.dat$regime !="HighPhaeo"] <=0.5,1,
                       ifelse(plot.dat$diatom[plot.dat$regime !="HighPhaeo"] > 0.5 & plot.dat$diatom[plot.dat$regime !="HighPhaeo"] <=0.75,1.25,1.5)))
in.dat = data.frame(y = a[plot.dat$regime !="HighPhaeo"] ,
                    x = sl[plot.dat$regime !="HighPhaeo"],
                    c = pch.in,
                    col =in.col.plot[plot.dat$regime !="HighPhaeo"])
in.dat = in.dat[in.dat$y>0,]
plot(y = in.dat$y , x=in.dat$x, 
     bg=in.dat$col,pch=21,cex=in.dat$c,ylim=c(0.01,15),xlim=c(-0.00258,-0.000043),
     col=alpha(in.dat$col,0.5),
     ylab= expression("Diatom"~" (mg m"^-3*")"),  log="y", xlab="")
a = ifelse(a>0,a,NA)
a = lm( log10(a[plot.dat$regime =="Shelf"]) ~sl[plot.dat$regime =="Shelf"])
summary(a)
abline(a,xpd=F,col = in.col[3],lty=2,lwd=2)
text(y=12,x = -0.0022,expression("R"^2*" = 0.15"), col="black",cex=1.2)
legend("bottomleft","(c)",bty="n")
rm(a,in.dat,plot.dat.norm,sl,sl2,pch.in)
#Row 2
wavelength = "wv675nm"
plot.dat.norm = plot.dat[,paste0("wv", 400:700,"nm")]/plot.dat[,wavelength]
sl = (plot.dat.norm$wv518nm - plot.dat.norm$wv471nm)/(518-471)
sl2=sl
boxplot(sl ~ as.factor(plot.dat$regime),col=in.col[c(2,1,4,3)],ylab=expression("Normalized to 675 nm a"['PHY']*"" ),
        ylim=c(-0.056,-0.010),
        xlab ="",horizontal=T,names=rev(c("DDS","MPB","HPB","LCB")))
        legend("bottomleft","(d)",bty="n")
a = plot.dat$phaeo*plot.dat$chla
pch.in = ifelse(plot.dat$phaeo[plot.dat$regime !="Shelf"] <=0.25,0.75,
                ifelse(plot.dat$phaeo[plot.dat$regime !="Shelf"] > 0.25 & plot.dat$phaeo[plot.dat$regime !="Shelf"] <=0.5,1,
                       ifelse(plot.dat$phaeo[plot.dat$regime !="Shelf"] > 0.5 & plot.dat$phaeo[plot.dat$regime !="Shelf"] <=0.75,1.25,1.5)))
in.dat = data.frame(y = a[plot.dat$regime !="Shelf"] ,
                    x = sl[plot.dat$regime !="Shelf"],
                    c = pch.in,
                    col =in.col.plot[plot.dat$regime !="Shelf"])
in.dat = in.dat[in.dat$y>0,]
plot(y = in.dat$y , x=in.dat$x,ylim=c(0.01,15), 
     bg=in.dat$col,pch=21,cex=in.dat$c,xlim=c(-0.056,-0.010),
     col=alpha(in.dat$col,0.5), 
     ylab=expression(italic("Phaeocystis")~" (mg m"^-3*")"), 
     xlab= "",log="y")
a = ifelse(a>0,a,NA)
a = lm( log10(a[plot.dat$regime =="HighPhaeo"]) ~sl[plot.dat$regime =="HighPhaeo"])
summary(a)
legend("bottomleft","(e)",bty="n")
a = plot.dat$diatom*plot.dat$chla
pch.in = ifelse(plot.dat$diatom[plot.dat$regime !="HighPhaeo"] <=0.25,0.75,
                ifelse(plot.dat$diatom[plot.dat$regime !="HighPhaeo"] > 0.25 & plot.dat$diatom[plot.dat$regime !="HighPhaeo"] <=0.5,1,
                       ifelse(plot.dat$diatom[plot.dat$regime !="HighPhaeo"] > 0.5 & plot.dat$diatom[plot.dat$regime !="HighPhaeo"] <=0.75,1.25,1.5)))
in.dat = data.frame(y = a[plot.dat$regime !="HighPhaeo"] ,
               x = sl[plot.dat$regime !="HighPhaeo"],
               c = pch.in,
               col =in.col.plot[plot.dat$regime !="HighPhaeo"])
in.dat = in.dat[in.dat$y>0,]
plot(y = in.dat$y , x=in.dat$x, 
     bg=in.dat$col,pch=21,cex=in.dat$c,ylim=c(0.01,15),xlim=c(-0.056,-0.010),
     col=alpha(in.dat$col,0.5),
     ylab= expression("Diatom"~" (mg m"^-3*")"),  log="y", xlab="")
a = ifelse(a>0,a,NA)
a = lm( log10(a[plot.dat$regime =="Shelf"]) ~sl[plot.dat$regime =="Shelf"])
summary(a)
abline(a,xpd=F,col = in.col[3],lty=2,lwd=2)
text(y=12,x = -0.05,expression("R"^2*" = 0.55"), col="black",cex=1.2)
legend("bottomleft","(f)",bty="n")
rm(a,in.dat,plot.dat.norm,sl,sl2,pch.in)
##Row 3
wavelength = "chla"
plot.dat.norm = plot.dat[,paste0("wv", 400:700,"nm")]/plot.dat[,wavelength]
sl = (plot.dat.norm$wv518nm - plot.dat.norm$wv471nm)/(518-471)
sl2=sl
boxplot(sl ~ as.factor(plot.dat$regime),col=in.col[c(2,1,4,3)],ylab=expression("a*"['PHY']*"" ),
        ylim=c(-0.0029,-0.00005),
        xlab =expression("S"['PHY']* " (470-518) (nm"^-1*")"),horizontal=T,names=rev(c("DDS","MPB","HPB","LCB")))
legend("bottomleft","(g)",bty="n")
a = plot.dat$phaeo*plot.dat$chla
pch.in = ifelse(plot.dat$phaeo[plot.dat$regime !="Shelf"] <=0.25,0.75,
                ifelse(plot.dat$phaeo[plot.dat$regime !="Shelf"] > 0.25 & plot.dat$phaeo[plot.dat$regime !="Shelf"] <=0.5,1,
                       ifelse(plot.dat$phaeo[plot.dat$regime !="Shelf"] > 0.5 & plot.dat$phaeo[plot.dat$regime !="Shelf"] <=0.75,1.25,1.5)))
in.dat = data.frame(y = a[plot.dat$regime !="Shelf"] ,
                    x = sl[plot.dat$regime !="Shelf"],
                    c = pch.in,
                    col =in.col.plot[plot.dat$regime !="Shelf"])
in.dat = in.dat[in.dat$y>0,]
plot(y = in.dat$y , x=in.dat$x,ylim=c(0.01,15), 
     bg=in.dat$col,pch=21,cex=in.dat$c,xlim=c(-0.0029,-0.00005),
     col=alpha(in.dat$col,0.5), 
     ylab=expression(italic("Phaeocystis")~" (mg m"^-3*")"), 
     xlab= expression("S"['PHY']* " (470-518) (nm"^-1*")"),log="y")#,xaxt="n")
a = ifelse(a>0,a,NA)
a = lm( log10(a[plot.dat$regime =="HighPhaeo"]) ~sl[plot.dat$regime =="HighPhaeo"])
summary(a)
legend("bottomleft","(h)",bty="n")
a = plot.dat$diatom*plot.dat$chla
pch.in = ifelse(plot.dat$diatom[plot.dat$regime !="HighPhaeo"] <=0.25,0.75,
                ifelse(plot.dat$diatom[plot.dat$regime !="HighPhaeo"] > 0.25 & plot.dat$diatom[plot.dat$regime !="HighPhaeo"] <=0.5,1,
                       ifelse(plot.dat$diatom[plot.dat$regime !="HighPhaeo"] > 0.5 & plot.dat$diatom[plot.dat$regime !="HighPhaeo"] <=0.75,1.25,1.5)))
in.dat = data.frame(y = a[plot.dat$regime !="HighPhaeo"] ,
                    x = sl[plot.dat$regime !="HighPhaeo"],
                    c = pch.in,
                    col =in.col.plot[plot.dat$regime !="HighPhaeo"])
in.dat = in.dat[in.dat$y>0,]
plot(y = in.dat$y , x=in.dat$x, 
     bg=in.dat$col,pch=21,cex=in.dat$c,xlim=c(-0.0029,-0.00005),
     col=alpha(in.dat$col,0.5),
     ylab= expression("Diatom"~" (mg m"^-3*")"),  log="y", xlab=expression("S"['PHY']* " (470-518) (nm"^-1*")"))
a = ifelse(a>0,a,NA)
a = lm( log10(a[plot.dat$regime =="Shelf"]) ~sl[plot.dat$regime =="Shelf"])
summary(a)
abline(a,xpd=F,col = in.col[3],lty=2,lwd=2)
text(y=12,x = -0.0025,expression("R"^2*" = 0.41"), col="black",cex=1.2)
legend("bottomleft","(i)",bty="n")
rm(a,in.dat,plot.dat.norm,sl,sl2,pch.in)
dev.off()
rm(plot.dat,in.col,in.col.lty,in.col.plot,wavelength)
