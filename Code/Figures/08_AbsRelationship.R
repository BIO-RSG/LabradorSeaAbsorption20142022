rm(list=ls())
library(pals)
library(scales)
library(oceancolouR)
options(scipen=5)
hplc = read.csv("./Data/PhytoplanktonGrouping.csv")
phyto = read.csv("./Data/Absorption_Phytoplankton.csv")
phyto = phyto[,c("SAMPLE_ID","wv443nm")]
names(phyto) = c("SAMPLE_ID","phyto")
hplc = merge(y=phyto , x = hplc, by.y="SAMPLE_ID", by.x="SAMPLE_ID")
phyto = read.csv("./Data/Absorption_Detritus.csv")
phyto = phyto[,c("SAMPLE_ID","wv443nm")]
names(phyto) = c("SAMPLE_ID","det")
hplc = merge(y=phyto , x = hplc, by.y="SAMPLE_ID", by.x="SAMPLE_ID",all.x=T)
phyto = read.csv("./Data/Absorption_CDOM.csv")
phyto = phyto[,c("SAMPLE_ID", "wv443nm")]
names(phyto) = c("SAMPLE_ID", "cdom")
hplc = merge(y=phyto , x = hplc, by.y="SAMPLE_ID", by.x="SAMPLE_ID",all.x=T)
plot.dat = hplc
rm(phyto,hplc)
png("./Figures/08_AbsRelationship.png",width=6.5,height=2.75, units = "in",res=300,pointsize = 10 )
par(mfrow=c(1,2),mar=c(3,3,1,1),mgp=c(2,1,0),oma=c(0,0.5,0,0),xpd=NA)
in.col = alpha(cubicl(4),0.8)
in.col.plot = ifelse(plot.dat$regime=="Basin" ,in.col[2],
                     ifelse(plot.dat$regime=="Shelf" ,in.col[3],
                            ifelse(plot.dat$regime=="HighPhaeo" ,in.col[1],in.col[4])))
#Det
plot(x=plot.dat$phyto, y = plot.dat$det,col=in.col.plot,pch=20,
     ylab=expression("a"['NAP']* " (443) (m"^-1*")"),xlab=expression("a"['PHY']* " (443) (m"^-1*")"))
bl=lm(plot.dat$det[plot.dat$regime=="Basin"]~plot.dat$phyto[plot.dat$regime=="Basin"])
abline(bl, col = in.col[2],lty=2,xpd=F)
sl=lm(plot.dat$det[plot.dat$regime=="Shelf"]~plot.dat$phyto[plot.dat$regime=="Shelf"])
abline(sl, col = in.col[3],lty=2,xpd=F)
hl=lm(plot.dat$det[plot.dat$regime=="HighPhaeo"]~plot.dat$phyto[plot.dat$regime=="HighPhaeo"])
abline(hl, col = in.col[1],lty=2,xpd=F)
ml=lm(plot.dat$det[plot.dat$regime=="MixPhaeo"]~plot.dat$phyto[plot.dat$regime=="MixPhaeo"])
abline(ml, col = in.col[4],lty=2,xpd=F)
legend("topleft","(a)",bty="n")
pd.all = data.frame(Intercept = c(bl$coefficients[1],sl$coefficients[1],hl$coefficients[1],ml$coefficients[1]),
                    Slope = c(bl$coefficients[2],sl$coefficients[2],hl$coefficients[2],ml$coefficients[2]),
                    r2 = c(summary(bl)$adj.r.squared,summary(sl)$adj.r.squared,summary(hl)$adj.r.squared,
                           summary(ml)$adj.r.squared),
                    pvalue = c(summary(bl)$coefficients[2,4],summary(sl)$coefficients[2,4],
                               summary(hl)$coefficients[2,4],summary(ml)$coefficients[2,4]))
row.names(pd.all)=c("Basin" , "Shelf"  ,  "HighPhaeo", "MixPhaeo")
legend("bottomright",legend= c("LCB","MPB","HPB","DDS"),col=in.col[c(2,4,1,3)],pch=20)
#CDOM
plot(x=plot.dat$phyto, y = plot.dat$cdom,col=in.col.plot,pch=20,ylab=expression("a"['CDOM']* " (443) (m"^-1*")"),
     xlab=expression("a"['PHY']* " (443) (m"^-1*")"))
bl=lm(plot.dat$cdom[plot.dat$regime=="Basin"]~plot.dat$phyto[plot.dat$regime=="Basin"])
abline(bl, col = in.col[2],lty=2,xpd=F)
sl=lm(plot.dat$cdom[plot.dat$regime=="Shelf"]~plot.dat$phyto[plot.dat$regime=="Shelf"])
abline(sl, col = in.col[3],lty=2,xpd=F)
hl=lm(plot.dat$cdom[plot.dat$regime=="HighPhaeo"]~plot.dat$phyto[plot.dat$regime=="HighPhaeo"])
abline(hl, col = in.col[1],lty=2,xpd=F)
ml=lm(plot.dat$cdom[plot.dat$regime=="MixPhaeo"]~plot.dat$phyto[plot.dat$regime=="MixPhaeo"])
abline(ml, col = in.col[4],lty=2,xpd=F)
legend("topleft","(b)",bty="n")
cd.all = data.frame(Intercept = c(bl$coefficients[1],sl$coefficients[1],hl$coefficients[1],ml$coefficients[1]),
                    Slope = c(bl$coefficients[2],sl$coefficients[2],hl$coefficients[2],ml$coefficients[2]),
                    r2 = c(summary(bl)$adj.r.squared,summary(sl)$adj.r.squared,summary(hl)$adj.r.squared,
                           summary(ml)$adj.r.squared),
                    pvalue = c(summary(bl)$coefficients[2,4],summary(sl)$coefficients[2,4],
                               summary(hl)$coefficients[2,4],summary(ml)$coefficients[2,4]))
row.names(cd.all)=c("Basin" , "Shelf" ,  "HighPhaeo" , "MixPhaeo")
dev.off()

rm(list=ls()[! ls() %in% c("cd.all","pd.all")])

tab = cbind(t(pd.all),t(cd.all))
tab[1,] = round(tab[1,],5)
tab[2,] = round(tab[2,],3)
tab[3,] = round(tab[3,],2)
tab[4,] = round(tab[4,],5)
tab = tab[,c(2,3,4,1,6,7,8,5)]
tab = rbind(c(rep("NAP",4),rep("CDOM",4)),tab)
colnames(tab)=c("DDS","HPB","MPB","LCB", "DDS","HPB","MPB","LCB")
write.csv(tab, "Tables/Table6.csv",row.names = T)
rm(tab,cd.all,pd.all)
