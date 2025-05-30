library(pals)
library(scales)
options(scipen=5)

hplc = read.csv("./Data/PhytoplanktonGrouping.csv")
##
#We want to sort the dataset from West to East by Year
lon = sort(hplc$STARTLON,index.return=T)$ix
hplc = hplc[lon,]
year = sort(hplc$YEAR,index.return=T)$ix
hplc = hplc[year,]
##
a = sort(unique(hplc$STATION))
a = data.frame(STATION =a)
lon.dist = hplc[duplicated(hplc$STATION)==F,c("STATION","STARTLON","STARTLAT")]
lon = sort(lon.dist$STARTLON,index.return=T)$ix
lon.dist =lon.dist[lon,]
dst=geosphere::distHaversine(lon.dist[,2:3])/1000
dst =c(0,dst)
dst.km = rep(NA, 48)
for (i in 1:48){dst.km[i]=   sum(dst[1:i])}
y.lab = ceiling(dst.km)
y.lab[1]="L3_00"
##

hplc = hplc[,names(hplc)[c(3,6,24:31)]]
in.col=brewer.spectral(8)[c(8:1)]
set.data = function(hplc, yr){
    plot.dat = hplc[hplc$YEAR==yr,]
    plot.dat = merge(plot.dat,a,all=T)
    plot.dat = plot.dat[duplicated(plot.dat$STATION)==F,]
    plot.dat = plot.dat[,c(10:3)]
    plot.dat = t(as.matrix(plot.dat))*100
    return(plot.dat)
}

png("./Figures/02_PhytoContribution.png",width=6.5,
    height=7, units = "in",res=300,pointsize = 10 )
par(mar=c(2.5,3,1,0),xpd=NA,mgp=c(2,1,0),family="serif",oma=c(4,0,0.5,0.5),mfrow=c(4,2))
plot.dat = set.data(hplc,2014)
b=barplot(plot.dat,horiz = F,legend=F,col=in.col,border=in.col,main="2014 (May)",
        xaxt="n",ylab="Relative Contribution (%)",ylim=c(-2,102))
box()
axis(1,at=b,labels=y.lab)
abline(v=c(9.7,52.9),lty=2,xpd=F,lwd=2)
legend(x=b[46],y=118,legend="(a)",bty="n")
plot.dat = set.data(hplc,2015)
barplot(plot.dat,horiz = F,legend=F,col=in.col,border=in.col,main="2015 (May)",
        xaxt="n",ylab="",ylim=c(-2,102))
box()
axis(1,at=b,labels=y.lab)
abline(v=c(9.7,52.9),lty=2,xpd=F,lwd=2)
legend(x=b[46],y=118,legend="(b)",bty="n")
plot.dat = set.data(hplc,2016)
barplot(plot.dat,horiz = F,legend=F,col=in.col,border=in.col,main="2016 (May)",
        xaxt="n",ylab="Relative Contribution (%)",ylim=c(-2,102))
box()
axis(1,at=b,labels=y.lab)
abline(v=c(9.7,52.9),lty=2,xpd=F,lwd=2)
legend(x=b[46],y=118,legend="(c)",bty="n")
plot.dat = set.data(hplc,2018)
barplot(plot.dat,horiz = F,legend=F,col=in.col,border=in.col,main="2018 (May)",
        xaxt="n",ylab="",ylim=c(-2,102))
box()
axis(1,at=b,labels=y.lab)
abline(v=c(9.7,52.9),lty=2,xpd=F,lwd=2)
legend(x=b[46],y=118,legend="(d)",bty="n")
plot.dat = set.data(hplc,2019)
barplot(plot.dat,horiz = F,legend=F,col=in.col,border=in.col,main="2019 (June)",
        xaxt="n",ylab="Relative Contribution (%)",ylim=c(-2,102))
box()
axis(1,at=b,labels=y.lab)
abline(v=c(9.7,52.9),lty=2,xpd=F,lwd=2)
legend(x=b[46],y=118,legend="(e)",bty="n")
plot.dat = set.data(hplc,2020)
barplot(plot.dat,horiz = F,legend=F,col=in.col,border=in.col,main="2020 (July-August)",
        xaxt="n",ylab="",ylim=c(-2,102))
box()
axis(1,at=b,labels=y.lab)
title(xlab="Distance from First Station (km)")
abline(v=c(9.7,52.9),lty=2,xpd=F,lwd=2)
legend(x=b[46],y=118,legend="(f)",bty="n")
plot.dat = set.data(hplc,2022)
barplot(plot.dat,horiz = F,legend=F,col=in.col,border=in.col,main="2022 (May)",
        xaxt="n",ylab="Relative Contribution (%)",ylim=c(-2,102))
box()
axis(1,at=b,labels=y.lab)
title(xlab="Distance from First Station (km)")
abline(v=c(9.7,52.9),lty=2,xpd=F,lwd=2)
legend(y=-28, x=b[6], legend=rownames(plot.dat),fill=in.col,xpd=NA,ncol=4,bty="n",title="Group")
legend(x=b[46],y=118,legend="(g)",bty="n")
##
new.dat =  read.csv("./Data/PhytoplanktonGrouping.csv")
in.col = alpha(cubicl(4),0.8)
in.col.p = ifelse(new.dat$regime=="Basin" ,in.col[2],
                     ifelse(new.dat$regime=="Shelf" ,in.col[3],
                            ifelse(new.dat$regime=="HighPhaeo" ,in.col[1],in.col[4])))
plot(y = new.dat$diatom*100, x=new.dat$phaeo*100,bg=in.col.p,pch=21,cex=1.2,
     xlab=expression(italic("Phaeocystis")~" (%)"), ylab="Diatom (%)")
legend(x=95,y=120,legend="(h)",bty="n")
legend(y=-28, x=35, legend=c("HPB","LCB","DDS","MPB"),fill=in.col,xpd=NA,ncol=2,bty="n",title="Regime")
dev.off()