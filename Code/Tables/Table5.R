hplc = read.csv("./Data/PhytoplanktonGrouping.csv")
phyto = read.csv("./Data/Absorption_Phytoplankton.csv")
plot.dat = merge(y=phyto , x = hplc, by.y="SAMPLE_ID", by.x="SAMPLE_ID")

##
sl1 = (plot.dat$wv518nm - plot.dat$wv471nm)/(518-471)
#
plot.dat.norm = plot.dat[,paste0("wv", 400:700,"nm")]/plot.dat[,"wv443nm"]
sl2 = (plot.dat.norm$wv518nm - plot.dat.norm$wv471nm)/(518-471)
##
plot.dat.norm = plot.dat[,paste0("wv", 400:700,"nm")]/plot.dat[,"wv675nm"]
sl3 = (plot.dat.norm$wv518nm - plot.dat.norm$wv471nm)/(518-471)
##
plot.dat.norm = plot.dat[,paste0("wv", 400:700,"nm")]/plot.dat[,"chla"]
sl4 = (plot.dat.norm$wv518nm - plot.dat.norm$wv471nm)/(518-471)
##
out.dat = data.frame(regime = plot.dat$regime, aphy = sl1, aphy443 = sl2,sphy665 = sl3,aspecphy = sl4)
tab.out = matrix(NA, nrow=4,ncol=4)
tab.out.l = matrix(NA, nrow=4,ncol=4)
tab.out.h = matrix(NA, nrow=4,ncol=4)
par(mfrow=c(2,2),mar=c(3,3,1,0),mgp=c(2,1,0))
for (i in 2:5){
  tab.out[i-1,] = round(tapply(out.dat[,i], out.dat$regime, "median"),5)
  tab.out.l[i-1,1] =round(boxplot(out.dat[out.dat$regime=="Basin",i],plot=F)$stats[c(2)],5)
  tab.out.h[i-1,1]=round(boxplot(out.dat[out.dat$regime=="Basin",i],plot=F)$stats[c(4)],5)
  tab.out.l[i-1,2] = round(boxplot(out.dat[out.dat$regime=="HighPhaeo",i],plot=F)$stats[c(2)],5)
  tab.out.h[i-1,2] = round(boxplot(out.dat[out.dat$regime=="HighPhaeo",i],plot=F)$stats[c(4)],5)
  tab.out.l[i-1,3] = round(boxplot(out.dat[out.dat$regime=="MixPhaeo",i],plot=F)$stats[c(2)],5)
  tab.out.h[i-1,3] =round(boxplot(out.dat[out.dat$regime=="MixPhaeo",i],plot=F)$stats[c(4)],5)
  tab.out.l[i-1,4] = round(boxplot(out.dat[out.dat$regime=="Shelf",i],plot=F)$stats[c(2)],5)
  tab.out.h[i-1,4] = round(boxplot(out.dat[out.dat$regime=="Shelf",i],plot=F)$stats[c(4)],5)
 }
colnames(tab.out) = c("LCB", "HPB","MPB","DDS")
row.names(tab.out) = names(out.dat)[2:5]
tab.out = tab.out[,c(4,2,3,1)]
colnames(tab.out.l) = c("LCB", "HPB","MPB","DDS")
row.names(tab.out.l) = names(out.dat)[2:5]
tab.out.l = tab.out.l[,c(4,2,3,1)]
colnames(tab.out.h) = c("LCB", "HPB","MPB","DDS")
row.names(tab.out.h) = names(out.dat)[2:5]
tab.out.h = tab.out.h[,c(4,2,3,1)]

pairwise.wilcox.test(out.dat$aphy, out.dat$regime,
                     p.adjust.method = "BH")
pairwise.wilcox.test(out.dat$aphy443, out.dat$regime,
                     p.adjust.method = "BH")
pairwise.wilcox.test(out.dat$sphy665, out.dat$regime,
                     p.adjust.method = "BH")
pairwise.wilcox.test(out.dat$aspecphy, out.dat$regime,
                     p.adjust.method = "BH")

tab = rbind(paste0(" " , tab.out[1,]," *", c(" A"," B"," AC"," D")),
      paste0("[",tab.out.l[1,]," - ", tab.out.h[1,],"]"), 
      paste0(" " ,tab.out[2,]," *", c(" A"," B"," B"," B")),
      paste0("[",tab.out.l[2,]," - ", tab.out.h[2,],"]"), 
      paste0(" " ,tab.out[3,]," *", c(" A"," A"," B"," C")),
      paste0("[",tab.out.l[3,]," - ", tab.out.h[3,],"]"), 
      paste0(" " ,tab.out[4,]," *", c(" A"," B"," C"," D")),
      paste0("[",tab.out.l[4,]," - ", tab.out.h[4,],"]"))
colnames(tab) = c("DDS","HPB","MPB","LCB")
row.names(tab) = c(rep("aphy",2),rep("aphy443",2), rep("aphy675",2),rep("a*phy",2))
write.csv(tab, "Tables/Table5.csv",row.names = T)
