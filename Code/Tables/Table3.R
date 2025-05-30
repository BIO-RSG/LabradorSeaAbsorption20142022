library(oceancolouR)
rgrlog10 = function(x,y){
  y = log10(y)
  x = log10(x)
  test = lmodel2::lmodel2(y ~ x)
  tdf = do.call(dplyr::bind_cols, oceancolouR::get_lm_stats(test,method="SMA")) 
  tdf[1:3] = round(tdf[1:3], 4)
  RMSE = round(rmse(x =x, y=y  ),4)
  tdf = cbind(tdf,RMSE)
  return(round(tdf,3))
}

hplc = read.csv("./Data/PhytoplanktonGrouping.csv")
phyto = read.csv("./Data/Absorption_Phytoplankton.csv")
phyto = phyto[,c("SAMPLE_ID","wv443nm")]
hplc = merge(y=phyto , x = hplc, by.y="SAMPLE_ID", by.x="SAMPLE_ID")

#chla averages
tab = hplc[,c( "regime", "chla")]
names(tab) = c("regime","y")
#
a = kruskal.test(y ~ regime, data = tab)
b = pairwise.wilcox.test(tab$y, tab$regime, p.adjust.method = "BH")
b = na.omit(as.vector(b$p.value))
chla= data.frame(LS.m =round(median(tab$y),3),
                 LS.l =round(boxplot(tab$y,plot=F)$stats[c(2)],3),
                 LS.h = round(boxplot(tab$y,plot=F)$stats[c(4)],3),
                 LCB.m = tapply(tab$y, list(tab$regime), median)[1],
                 LCB.l =round(boxplot(tab$y[tab$regime=="Basin"],plot=F)$stats[c(2)],3),
                 LCB.h =round(boxplot(tab$y[tab$regime=="Basin"],plot=F)$stats[c(4)],3),
                 HPB.m = tapply(tab$y, list(tab$regime), median) [2],
                 HPB.l = round(boxplot(tab$y[tab$regime=="HighPhaeo"],plot=F)$stats[c(2)],3),
                 HPB.h = round(boxplot(tab$y[tab$regime=="HighPhaeo"],plot=F)$stats[c(4)],3),
                 MPB.m = tapply(tab$y, list(tab$regime), median)[3],
                 MPB.l = round(boxplot(tab$y[tab$regime=="MixPhaeo"],plot=F)$stats[c(2)],3),
                 MPB.h =round(boxplot(tab$y[tab$regime=="MixPhaeo"],plot=F)$stats[c(4)],3),
                 DDS.m = tapply(tab$y, list(tab$regime), median)[4],
                 DDS.l = round(boxplot(tab$y[tab$regime=="Shelf"],plot=F)$stats[c(2)],3),
                 DDS.h = round(boxplot(tab$y[tab$regime=="Shelf"],plot=F)$stats[c(4)],3),
                 chi.r = a$statistic,
                 chi.df = a$parameter,
                 chi.p = a$p.value,
                 LCB.HPB=b[1],LCB.MPB=b[2],LCB.DDS=b[2],HPB.MPB=b[4],HPB.DDS=b[5],MPB.DDS=b[6] )
rm(tab,a,b)
##
#Diatom averages
tab = hplc[,c( "regime", "diatom")]
tab$diatom = tab$diatom*100
names(tab) = c("regime","y")
#
a = kruskal.test(y ~ regime, data = tab)
b = pairwise.wilcox.test(tab$y, tab$regime, p.adjust.method = "BH")
b = na.omit(as.vector(b$p.value))
diatom= data.frame(LS.m =round(median(tab$y),3),
                 LS.l =round(boxplot(tab$y,plot=F)$stats[c(2)],3),
                 LS.h = round(boxplot(tab$y,plot=F)$stats[c(4)],3),
                 LCB.m = tapply(tab$y, list(tab$regime), median)[1],
                 LCB.l =round(boxplot(tab$y[tab$regime=="Basin"],plot=F)$stats[c(2)],3),
                 LCB.h =round(boxplot(tab$y[tab$regime=="Basin"],plot=F)$stats[c(4)],3),
                 HPB.m = tapply(tab$y, list(tab$regime), median) [2],
                 HPB.l = round(boxplot(tab$y[tab$regime=="HighPhaeo"],plot=F)$stats[c(2)],3),
                 HPB.h = round(boxplot(tab$y[tab$regime=="HighPhaeo"],plot=F)$stats[c(4)],3),
                 MPB.m = tapply(tab$y, list(tab$regime), median)[3],
                 MPB.l = round(boxplot(tab$y[tab$regime=="MixPhaeo"],plot=F)$stats[c(2)],3),
                 MPB.h =round(boxplot(tab$y[tab$regime=="MixPhaeo"],plot=F)$stats[c(4)],3),
                 DDS.m = tapply(tab$y, list(tab$regime), median)[4],
                 DDS.l = round(boxplot(tab$y[tab$regime=="Shelf"],plot=F)$stats[c(2)],3),
                 DDS.h = round(boxplot(tab$y[tab$regime=="Shelf"],plot=F)$stats[c(4)],3),
                 chi.r = a$statistic,
                 chi.df = a$parameter,
                 chi.p = a$p.value,
                 LCB.HPB=b[1],LCB.MPB=b[2],LCB.DDS=b[2],HPB.MPB=b[4],HPB.DDS=b[5],MPB.DDS=b[6] )
rm(tab,a,b)
##
#Phaeo averages
tab = hplc[,c( "regime", "phaeo")]
tab$phaeo = tab$phaeo*100
names(tab) = c("regime","y")
#
a = kruskal.test(y ~ regime, data = tab)
b = pairwise.wilcox.test(tab$y, tab$regime, p.adjust.method = "BH")
b = na.omit(as.vector(b$p.value))
phaeo= data.frame(LS.m =round(median(tab$y),3),
                   LS.l =round(boxplot(tab$y,plot=F)$stats[c(2)],3),
                   LS.h = round(boxplot(tab$y,plot=F)$stats[c(4)],3),
                   LCB.m = tapply(tab$y, list(tab$regime), median)[1],
                   LCB.l =round(boxplot(tab$y[tab$regime=="Basin"],plot=F)$stats[c(2)],3),
                   LCB.h =round(boxplot(tab$y[tab$regime=="Basin"],plot=F)$stats[c(4)],3),
                   HPB.m = tapply(tab$y, list(tab$regime), median) [2],
                   HPB.l = round(boxplot(tab$y[tab$regime=="HighPhaeo"],plot=F)$stats[c(2)],3),
                   HPB.h = round(boxplot(tab$y[tab$regime=="HighPhaeo"],plot=F)$stats[c(4)],3),
                   MPB.m = tapply(tab$y, list(tab$regime), median)[3],
                   MPB.l = round(boxplot(tab$y[tab$regime=="MixPhaeo"],plot=F)$stats[c(2)],3),
                   MPB.h =round(boxplot(tab$y[tab$regime=="MixPhaeo"],plot=F)$stats[c(4)],3),
                   DDS.m = tapply(tab$y, list(tab$regime), median)[4],
                   DDS.l = round(boxplot(tab$y[tab$regime=="Shelf"],plot=F)$stats[c(2)],3),
                   DDS.h = round(boxplot(tab$y[tab$regime=="Shelf"],plot=F)$stats[c(4)],3),
                   chi.r = a$statistic,
                   chi.df = a$parameter,
                   chi.p = a$p.value,
                   LCB.HPB=b[1],LCB.MPB=b[2],LCB.DDS=b[2],HPB.MPB=b[4],HPB.DDS=b[5],MPB.DDS=b[6] )
rm(tab,a,b)
##
out.data = rbind(chla,diatom,phaeo)
row.names(out.data)=c("chla","diatom","phaeo")
tab = rbind(round(out.data[,"LS.m"],2),paste0("[", round(out.data[,"LS.l"],2)," - ", round(out.data[,"LS.h"],2),"]"),
            paste0(round(out.data[,"DDS.m"],2)," *",c(" A"," A"," A")),
            paste0("[", round(out.data[,"DDS.l"],2), " - ", round(out.data[,"DDS.h"],2),"]"),
            paste0(round(out.data[,"HPB.m"],2)," *",c(" AB"," B"," B")),
            paste0("[", round(out.data[,"HPB.l"],2)," - ", round(out.data[,"HPB.h"],2),"]"),
            paste0(round(out.data[,"MPB.m"],2)," *",c(" AC"," B"," C")),
            paste0("[", round(out.data[,"MPB.l"],2), " - ", round(out.data[,"MPB.h"],2),"]"),
            paste0(round(out.data[,"LCB.m"],2)," *",c(" D"," B"," A")),
            paste0("[", round(out.data[,"LCB.l"],2), " - ", round(out.data[,"LCB.h"],2),"]"))
row.names(tab)=c(rep("All",2),rep("DDS",2),rep("HPB",2),rep("MPB",2),rep("LCB",2))
colnames(tab) = c("chla","diatom","phaeo")
tab = as.data.frame(tab)
rm(chla,diatom,phaeo)
#
#
power.law = rbind(all = rgrlog10(x = hplc$chla,y= hplc$wv443)[1:3],
                  dds = rgrlog10(x = hplc$chla[hplc$regime=="Shelf"],y= hplc$wv443[hplc$regime=="Shelf"])[1:3],
                  hpb = rgrlog10(x = hplc$chla[hplc$regime=="HighPhaeo"],y= hplc$wv443[hplc$regime=="HighPhaeo"])[1:3],
                  mpb = rgrlog10(x = hplc$chla[hplc$regime=="MixPhaeo"],y= hplc$wv443[hplc$regime=="MixPhaeo"])[1:3],
                  lcb = rgrlog10(x = hplc$chla[hplc$regime=="Basin"],y= hplc$wv443[hplc$regime=="Basin"])[1:3])
power.law = (cbind(paste0( round(10^(power.law$Intercept),4), " x chl-a ^", round(power.law$Slope,3)),
      paste0("R^2 = ", round(power.law$Rsquared,2))))
tab$PowerLaw = c(power.law[1,1],power.law[1,2],power.law[2,1],power.law[2,2],power.law[3,1],power.law[3,2],
                 power.law[4,1],power.law[4,2],power.law[5,1],power.law[5,2])
rm(hplc,out.data,phyto,power.law,rgrlog10)
write.csv(tab, "Tables/Table3.csv",row.names = T)
   