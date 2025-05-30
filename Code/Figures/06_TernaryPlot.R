library(pals)
library(scales)
library(oceancolouR)
library(Ternary)
options(scipen=5)
#Ternary Plot
png("./Figures/06_TernaryPlot.png",
    width=6.5,height=6.5, units = "in",res=300,pointsize = 10 )
par(mar=c(0,0,0,0),xpd=NA,mgp=c(2,1,0),family="serif",oma=c(2,0,0,0),mfrow=c(2,2))

#TernaryPlot
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
#
#Determine the absorption contribution by constituent 
plot.dat$Total = plot.dat$det + plot.dat$phyto +plot.dat$cdom
plot.dat$DetPer = plot.dat$det/plot.dat$Total *100
plot.dat$PhytoPer = plot.dat$phyto/plot.dat$Total *100
plot.dat$CDOMPer = plot.dat$cdom/plot.dat$Total *100
##
in.plot = plot.dat
in.plot = na.omit(in.plot)
abs.list = as.list(as.data.frame(t(in.plot[,c("CDOMPer","PhytoPer" ,"DetPer")])))
TernaryPlot(blab=expression("a"['PHY']* " (443) (%)"), 
            clab = expression("a"['NAP']* " (443) (%)"), 
            alab=expression("a"['CDOM']* " (443) (%)"),cex=1,padding=0.01,lab.offset = 0.11)
in.col = alpha(cubicl(4),0.8)
in.col.plot = ifelse(in.plot$regime=="Basin" ,in.col[2],
                     ifelse(in.plot$regime=="Shelf" ,in.col[3],
                            ifelse(in.plot$regime=="HighPhaeo" ,in.col[1],in.col[4])))
AddToTernary(points, abs.list,bg=c(in.col.plot),pch=21,col = "grey30",cex=1.2)
str.in = as.list(as.data.frame(t(cbind(tapply(in.plot$CDOMPer,in.plot$regime, median),
                    tapply(in.plot$PhytoPer,in.plot$regime, median),
                    tapply(in.plot$DetPer,in.plot$regime, median)))))
AddToTernary(points, str.in, cex=2, lwd=2,lty=4,bg=in.col[c(2,1,4,3)],pch=21,col="grey")
legend(y=0.9,x=0.3, legend="(a)",bty="n",cex=1.2)
#
#TernaryPlot
##
wavelength = "wv470nm"
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
#
#Determine the absorption contribution by constituent 
plot.dat$Total = plot.dat$det + plot.dat$phyto +plot.dat$cdom
plot.dat$DetPer = plot.dat$det/plot.dat$Total *100
plot.dat$PhytoPer = plot.dat$phyto/plot.dat$Total *100
plot.dat$CDOMPer = plot.dat$cdom/plot.dat$Total *100
##
in.plot = plot.dat
in.plot = na.omit(in.plot)
abs.list = as.list(as.data.frame(t(in.plot[,c("CDOMPer","PhytoPer" ,"DetPer")])))
TernaryPlot(blab=expression("a"['PHY']* " (443) (%)"), 
            clab = expression("a"['NAP']* " (443) (%)"), 
            alab=expression("a"['CDOM']* " (443) (%)"),cex=1,padding=0.01,lab.offset = 0.11)
in.col = alpha(cubicl(4),0.8)
in.col.plot = ifelse(in.plot$regime=="Basin" ,in.col[2],
                     ifelse(in.plot$regime=="Shelf" ,in.col[3],
                            ifelse(in.plot$regime=="HighPhaeo" ,in.col[1],in.col[4])))
AddToTernary(points, abs.list,bg=c(in.col.plot),pch=21,col = "grey30",cex=1.2)
str.in = as.list(as.data.frame(t(cbind(tapply(in.plot$CDOMPer,in.plot$regime, median),
                                       tapply(in.plot$PhytoPer,in.plot$regime, median),
                                       tapply(in.plot$DetPer,in.plot$regime, median)))))
AddToTernary(points, str.in, cex=2, lwd=2,lty=4,bg=in.col[c(2,1,4,3)],pch=21,col="grey")
legend(y=0.9,x=0.3, legend="(b)",bty="n",cex=1.2)
#
#TernaryPlot
##
wavelength = "wv570nm"
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
#
#Determine the absorption contribution by constituent 
plot.dat$Total = plot.dat$det + plot.dat$phyto +plot.dat$cdom
plot.dat$DetPer = plot.dat$det/plot.dat$Total *100
plot.dat$PhytoPer = plot.dat$phyto/plot.dat$Total *100
plot.dat$CDOMPer = plot.dat$cdom/plot.dat$Total *100
##
in.plot = plot.dat
in.plot = na.omit(in.plot)
abs.list = as.list(as.data.frame(t(in.plot[,c("CDOMPer","PhytoPer" ,"DetPer")])))
TernaryPlot(blab=expression("a"['PHY']* " (443) (%)"), 
            clab = expression("a"['NAP']* " (443) (%)"), 
            alab=expression("a"['CDOM']* " (443) (%)"),cex=1,padding=0.01,lab.offset = 0.11)
in.col = alpha(cubicl(4),0.8)
in.col.plot = ifelse(in.plot$regime=="Basin" ,in.col[2],
                     ifelse(in.plot$regime=="Shelf" ,in.col[3],
                            ifelse(in.plot$regime=="HighPhaeo" ,in.col[1],in.col[4])))
AddToTernary(points, abs.list,bg=c(in.col.plot),pch=21,col = "grey30",cex=1.2)
str.in = as.list(as.data.frame(t(cbind(tapply(in.plot$CDOMPer,in.plot$regime, median),
                                       tapply(in.plot$PhytoPer,in.plot$regime, median),
                                       tapply(in.plot$DetPer,in.plot$regime, median)))))
AddToTernary(points, str.in, cex=2, lwd=2,lty=4,bg=in.col[c(2,1,4,3)],pch=21,col="grey")
legend(y=0.9,x=0.3, legend="(c)",bty="n",cex=1.2)
#
#TernaryPlot
##
wavelength = "wv675nm"
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
#
#Determine the absorption contribution by constituent 
plot.dat$Total = plot.dat$det + plot.dat$phyto +plot.dat$cdom
plot.dat$DetPer = plot.dat$det/plot.dat$Total *100
plot.dat$PhytoPer = plot.dat$phyto/plot.dat$Total *100
plot.dat$CDOMPer = plot.dat$cdom/plot.dat$Total *100
##
in.plot = plot.dat
in.plot = na.omit(in.plot)
abs.list = as.list(as.data.frame(t(in.plot[,c("CDOMPer","PhytoPer" ,"DetPer")])))
TernaryPlot(blab=expression("a"['PHY']* " (443) (%)"), 
            clab = expression("a"['NAP']* " (443) (%)"), 
            alab=expression("a"['CDOM']* " (443) (%)"),cex=1,padding=0.01,lab.offset = 0.11)
in.col = alpha(cubicl(4),0.8)
in.col.plot = ifelse(in.plot$regime=="Basin" ,in.col[2],
                     ifelse(in.plot$regime=="Shelf" ,in.col[3],
                            ifelse(in.plot$regime=="HighPhaeo" ,in.col[1],in.col[4])))
AddToTernary(points, abs.list,bg=c(in.col.plot),pch=21,col = "grey30",cex=1.2)
str.in = as.list(as.data.frame(t(cbind(tapply(in.plot$CDOMPer,in.plot$regime, median),
                                       tapply(in.plot$PhytoPer,in.plot$regime, median),
                                       tapply(in.plot$DetPer,in.plot$regime, median)))))
AddToTernary(points, str.in, cex=2, lwd=2,lty=4,bg=in.col[c(2,1,4,3)],pch=21,col="grey")
legend(y=0.9,x=0.3, legend="(d)",bty="n",cex=1.2)
legend(x=-1.0,y=-0.08,legend=c("DDS","LCB", "HPB","MPB"),col = "grey30",pt.bg=in.col[c(3,2,1,4)],
       pch=21, xpd=NA,ncol=4,bty="n",title="Regime",cex=1.2)
dev.off()