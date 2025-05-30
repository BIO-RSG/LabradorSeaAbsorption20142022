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
    chla= data.frame(LS.m = mean(tab$y),
                 LS.sd = sd(tab$y),
                 LCB.m = tapply(tab$y, list(tab$regime), mean)[1],
                 LCB.sd =tapply(tab$y, list(tab$regime), sd)[1],
                 HPB.m = tapply(tab$y, list(tab$regime), mean) [2],
                 HPB.sd = tapply(tab$y, list(tab$regime), sd) [2],
                 MPB.m = tapply(tab$y, list(tab$regime), mean)[3],
                 MPB.sd = tapply(tab$y, list(tab$regime), sd)[3],
                 DDS.m = tapply(tab$y, list(tab$regime), mean)[4],
                 DDS.sd = tapply(tab$y, list(tab$regime), sd)[4])
      return (chla)}

#aphy
tab = hplc[,c( "regime", "aphy")]
names(tab) = c("regime","y")
aphy =round(get.data(tab),3)
#a specific phy
tab = hplc[,c( "regime", "aphy","chla")]
tab$y = tab$aphy/tab$chla
tab = tab[,c("regime","y")]
aspecphy =round(get.data(tab),3)
#acdom
tab = hplc[,c( "regime", "acdom","SAMPLE_ID")]
tab = na.omit(tab)
names(tab) = c("regime","y")
acdom = round(get.data(tab),3)
#anap
tab = hplc[,c( "regime", "anap")]
tab = na.omit(tab)
names(tab) = c("regime","y")
anap = round(get.data(tab),3)
#slope
tab = hplc[,c( "regime", "slope350400")]
tab = na.omit(tab)
names(tab) = c("regime","y")
aslope = round(get.data(tab),3)
#a cdom phy
tab = hplc[,c( "regime", "aphy","acdom")]
tab = na.omit(tab)
tab$y = tab$acdom/tab$aphy
tab = tab[,c("regime","y")]
acdomphy=round(get.data(tab),2)
#chla
tab = hplc[,c( "regime", "chla")]
names(tab) = c("regime","y")
chla=round(get.data(tab),2)
#diatom
tab = hplc[,c( "regime", "diatom")]
tab$diatom = tab$diatom*100
names(tab) = c("regime","y")
diatom = round(get.data(tab),2)
#phaeo
tab = hplc[,c( "regime", "phaeo")]
tab$phaeo = tab$phaeo*100
names(tab) = c("regime","y")
phaeo = round(get.data(tab),2)

##
out.data = rbind(chla,diatom, phaeo, aphy,aspecphy,acdom,anap,aslope,acdomphy )
row.names(out.data)=c("chla", "diatom", "phaeo", "aphy","aspecphy","acdom","anap","aslope","acdomphy")
rm(chla,diatom, phaeo,aphy,aspecphy,acdom,anap,aslope,acdomphy,tab,hplc )
tab = cbind(paste0(out.data[,1]," ± " ,out.data[,2]),
            paste0(out.data[,3]," ± " ,out.data[,4]),
            paste0(out.data[,5]," ± " ,out.data[,6]),
            paste0(out.data[,7]," ± " ,out.data[,8]),
            paste0(out.data[,9]," ± " ,out.data[,10]))
colnames(tab) = c("All", "LCB", "HPB","MPB","DDS")
row.names(tab)=c("chla", "diatom", "phaeo", "aphy","aspecphy","acdom","anap","aslope","acdomphy")
tab = tab[,c(1,5,3,4,2)]
#
#
write.csv(tab, "Tables/TableS1.csv",row.names = T)
   