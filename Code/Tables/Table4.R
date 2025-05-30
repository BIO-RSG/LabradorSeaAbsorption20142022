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

get.data = function (tab){
  a = kruskal.test(y ~ regime, data = tab)
  b = pairwise.wilcox.test(tab$y, tab$regime, p.adjust.method = "BH")
  b = na.omit(as.vector(b$p.value))
  chla= data.frame(LS.m =round(median(tab$y),4),
                 LS.l =round(boxplot(tab$y,plot=F)$stats[c(2)],4),
                 LS.h = round(boxplot(tab$y,plot=F)$stats[c(4)],4),
                 LCB.m = tapply(tab$y, list(tab$regime), median)[1],
                 LCB.l =round(boxplot(tab$y[tab$regime=="Basin"],plot=F)$stats[c(2)],4),
                 LCB.h =round(boxplot(tab$y[tab$regime=="Basin"],plot=F)$stats[c(4)],4),
                 HPB.m = tapply(tab$y, list(tab$regime), median) [2],
                 HPB.l = round(boxplot(tab$y[tab$regime=="HighPhaeo"],plot=F)$stats[c(2)],4),
                 HPB.h = round(boxplot(tab$y[tab$regime=="HighPhaeo"],plot=F)$stats[c(4)],4),
                 MPB.m = tapply(tab$y, list(tab$regime), median)[3],
                 MPB.l = round(boxplot(tab$y[tab$regime=="MixPhaeo"],plot=F)$stats[c(2)],4),
                 MPB.h =round(boxplot(tab$y[tab$regime=="MixPhaeo"],plot=F)$stats[c(4)],4),
                 DDS.m = tapply(tab$y, list(tab$regime), median)[4],
                 DDS.l = round(boxplot(tab$y[tab$regime=="Shelf"],plot=F)$stats[c(2)],4),
                 DDS.h = round(boxplot(tab$y[tab$regime=="Shelf"],plot=F)$stats[c(4)],4),
                 chi.r = a$statistic,
                 chi.df = a$parameter,
                 chi.p = a$p.value,
                 LCB.HPB=b[1],LCB.MPB=b[2],LCB.DDS=b[2],HPB.MPB=b[4],HPB.DDS=b[5],MPB.DDS=b[6] )
      return (chla)}

#aphy
tab = hplc[,c( "regime", "aphy")]
names(tab) = c("regime","y")
aphy = get.data(tab)
#a specific phy
tab = hplc[,c( "regime", "aphy","chla")]
tab$y = tab$aphy/tab$chla
tab = tab[,c("regime","y")]
aspecphy = get.data(tab)
#acdom
tab = hplc[,c( "regime", "acdom","SAMPLE_ID")]
tab = na.omit(tab)
names(tab) = c("regime","y")
acdom = get.data(tab)
#anap
tab = hplc[,c( "regime", "anap")]
tab = na.omit(tab)
names(tab) = c("regime","y")
anap = get.data(tab)
#slope
tab = hplc[,c( "regime", "slope350400")]
tab = na.omit(tab)
names(tab) = c("regime","y")
aslope = get.data(tab)
#a specific phy
tab = hplc[,c( "regime", "aphy","acdom")]
tab = na.omit(tab)
tab$y = tab$acdom/tab$aphy
tab = tab[,c("regime","y")]
acdomphy= get.data(tab)

##
out.data = rbind(aphy,aspecphy,acdom,anap,aslope,acdomphy )
rm(aphy,aspecphy,acdom,anap,aslope,acdomphy,tab,hplc )
row.names(out.data)=c("aphy","aspecphy","acdom","anap","aslope","acdomphy")
tab = rbind(round(out.data[,"LS.m"],3),paste0("[", round(out.data[,"LS.l"],3)," - ", round(out.data[,"LS.h"],3),"]"),
            paste0(round(out.data[,"DDS.m"],3)," *",c(" A"," A"," A"," A"," ns", " A")),
            paste0("[", round(out.data[,"DDS.l"],3), " - ", round(out.data[,"DDS.h"],3),"]"),
            paste0(round(out.data[,"HPB.m"],3)," *",c(" B"," B"," B"," A"," ns", " B")),
            paste0("[", round(out.data[,"HPB.l"],3)," - ", round(out.data[,"HPB.h"],3),"]"),
            paste0(round(out.data[,"MPB.m"],3)," *",c(" A"," C"," B"," B"," ns", " A")),
            paste0("[", round(out.data[,"MPB.l"],3), " - ", round(out.data[,"MPB.h"],3),"]"),
            paste0(round(out.data[,"LCB.m"],3)," *",c(" C"," D"," AB"," B"," ns", " AB")),
            paste0("[", round(out.data[,"LCB.l"],3), " - ", round(out.data[,"LCB.h"],3),"]"))
row.names(tab)=c(rep("All",2),rep("DDS",2),rep("HPB",2),rep("MPB",2),rep("LCB",2))
colnames(tab)= c("aphy","aspecphy","acdom","anap","aslope","acdomphy")
tab = as.data.frame(tab)
#
#
write.csv(tab, "Tables/Table4.csv",row.names = T)
   