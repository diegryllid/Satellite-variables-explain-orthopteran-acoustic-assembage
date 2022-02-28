library(tidyverse)
library(dplyr)
library(activity)
library(overlap)

replaceNA<- function(x) { x[is.na(x)] <- 0; x }

#meses a graficar
mes<-c(4)

#directorio RFM
wdpmatching<-"D:/Users/dieag/Documents/Ongoing side projects/Community Yataros/Tesis/PeerJ/Results/detections/pattern matching"
wdrforests<- "D:/Users/dieag/Documents/Ongoing side projects/Community Yataros/Tesis/PeerJ/Results/detections/random forests"
setwd(wdrforests)

#GR2
gr2<-read.csv("gr2 revisado.csv")
gr2 <- gr2 %>% filter(presence==1) %>% filter(month<=mes)
gr2$datetime<-ISOdatetime(gr2$year,gr2$month,gr2$day,gr2$hour,gr2$minute,":00",tz="America/Bogota")
gr2$sp<-"gr2"
gr2<-select(gr2,datetime,site,sp)
gr2<-gettime(gr2$datetime, scale = "radian")

#GR4
gr4<-read.csv("gr4 revisado.csv")
gr4 <- gr4 %>% filter(presence==1) %>% filter(month<=mes)
gr4$datetime<-ISOdatetime(gr4$year,gr4$month,gr4$day,gr4$hour,gr4$minute,":00",tz="America/Bogota")
gr4$sp<-"gr4"
gr4<-select(gr4,datetime,site,sp)
gr4<-gettime(gr4$datetime, scale = "radian")

#GR12
gr12<-read.csv("gr12 revisado.csv")
gr12 <- gr12 %>% filter(presence==1) %>% filter(month<=mes)
gr12$datetime<-ISOdatetime(gr12$year,gr12$month,gr12$day,gr12$hour,gr12$minute,":00",tz="America/Bogota")
gr12$sp<-"gr12"
gr12<-select(gr12,datetime,site,sp)
gr12<-gettime(gr12$datetime, scale = "radian")

#GR22
gr22<-read.csv("gr22 revisado.csv")
gr22 <- gr22 %>% filter(presence==1) %>% filter(month<=mes)
gr22$datetime<-ISOdatetime(gr22$year,gr22$month,gr22$day,gr22$hour,gr22$minute,":00",tz="America/Bogota")
gr22$sp<-"gr22"
gr22<-select(gr22,datetime,site,sp)
gr22<-gettime(gr22$datetime, scale = "radian")

#directorio PATTERN MATCHING
setwd(wdpmatching)

#GR8
gr8<-read.csv("gr8.csv")
gr8 <- gr8 %>% filter(validated=="present") %>% filter(month<=mes) 
gr8$datetime<-ISOdatetime(gr8$year,gr8$month,gr8$day,gr8$hour,gr8$min,":00",tz="America/Bogota")
gr8$sp<-"gr8"
gr8<-select(gr8,datetime,site,sp)
gr8<-gettime(gr8$datetime, scale = "radian")

#gr13
gr13<-read.csv("gr13.csv")
gr13 <- gr13 %>% filter(validated=="present") %>% filter(month<=mes) 
gr13$datetime<-ISOdatetime(gr13$year,gr13$month,gr13$day,gr13$hour,gr13$min,":00",tz="America/Bogota")
gr13$sp<-"gr13"
gr13<-select(gr13,datetime,site,sp)
gr13<-gettime(gr13$datetime, scale = "radian")

#gr20
gr20<-read.csv("gr20.csv")
gr20 <- gr20 %>% filter(validated=="present") %>% filter(month<=mes) 
gr20$datetime<-ISOdatetime(gr20$year,gr20$month,gr20$day,gr20$hour,gr20$min,":00",tz="America/Bogota")
gr20$sp<-"gr20"
gr20<-select(gr20,datetime,site,sp)
gr20<-gettime(gr20$datetime, scale = "radian")


#pair kernel estimator
ovlp<-setNames(data.frame(matrix(ncol = 6, nrow = 5)), c("Gr4","Gr8","Gr12","Gr13","Gr22","Gr20")) #create empty dataframe
rownames(ovlp) <- c("Gr2", "Gr4", "Gr8","Gr12","Gr13")
ovlp["Gr2","Gr4"]<-overlapEst(gr2, gr4, type="Dhat4", adjust=1)
ovlp["Gr2","Gr8"]<-overlapEst(gr2, gr8, type="Dhat4", adjust=1)
ovlp["Gr2","Gr12"]<-overlapEst(gr2, gr12, type="Dhat4", adjust=1)
ovlp["Gr2","Gr13"]<-overlapEst(gr2, gr13, type="Dhat4", adjust=1)
ovlp["Gr2","Gr22"]<-overlapEst(gr2, gr22, type="Dhat4", adjust=1)
ovlp["Gr2","Gr20"]<-overlapEst(gr2, gr20, type="Dhat4", adjust=1)
ovlp["Gr4","Gr8"]<-overlapEst(gr4, gr8, type="Dhat4", adjust=1)
ovlp["Gr4","Gr12"]<-overlapEst(gr4, gr12, type="Dhat4", adjust=1)
ovlp["Gr4","Gr13"]<-overlapEst(gr4, gr13, type="Dhat4", adjust=1)
ovlp["Gr4","Gr22"]<-overlapEst(gr4, gr22, type="Dhat4", adjust=1)
ovlp["Gr4","Gr20"]<-overlapEst(gr4, gr20, type="Dhat4", adjust=1)
ovlp["Gr8","Gr12"]<-overlapEst(gr8, gr12, type="Dhat4", adjust=1)
ovlp["Gr8","Gr13"]<-overlapEst(gr8, gr13, type="Dhat4", adjust=1)
ovlp["Gr8","Gr22"]<-overlapEst(gr8, gr22, type="Dhat4", adjust=1)
ovlp["Gr8","Gr20"]<-overlapEst(gr8, gr20, type="Dhat4", adjust=1)
ovlp["Gr12","Gr13"]<-overlapEst(gr12, gr13, type="Dhat4", adjust=1)
ovlp["Gr12","Gr22"]<-overlapEst(gr12, gr22, type="Dhat4", adjust=1)
ovlp["Gr12","Gr20"]<-overlapEst(gr12, gr20, type="Dhat4", adjust=1)
ovlp["Gr13","Gr22"]<-overlapEst(gr13, gr22, type="Dhat4", adjust=1)
ovlp["Gr13","Gr20"]<-overlapEst(gr13, gr20, type="Dhat4", adjust=1)

#Bootstrap estimator
gr2boot <- resample(gr2, 10000)
gr4boot <- resample(gr4, 10000)
gr8boot <- resample(gr8, 10000)
gr12boot <- resample(gr12, 10000)
gr13boot <- resample(gr13, 10000)
gr22boot <- resample(gr22, 10000)
gr20boot <- resample(gr20, 10000)

#confidence interval at 95%
boothigh<-setNames(data.frame(matrix(ncol = 6, nrow = 5)), c("Gr4","Gr8","Gr12","Gr13","Gr22","Gr20")) #create empty dataframe
rownames(boothigh) <- c("Gr2", "Gr4", "Gr8","Gr12","Gr13")
bootlow<-setNames(data.frame(matrix(ncol = 6, nrow = 5)), c("Gr4","Gr8","Gr12","Gr13","Gr22","Gr20")) #create empty dataframe
rownames(bootlow) <- c("Gr2", "Gr4", "Gr8","Gr12","Gr13")

tmp <- bootEst(gr2boot, gr4boot, adjust = c(NA, 1, NA))
tmp <- tmp[, 2] # Extract the required column of the matrix
tmp<-bootCI(ovlp["Gr2","Gr4"], tmp)
boothigh["Gr2","Gr4"]<-tmp[4, 2] #extract upper interval from matrix
bootlow["Gr2","Gr4"]<-tmp[4, 1] #extract lower interval from matrix

tmp <- bootEst(gr2boot, gr8boot, adjust = c(NA, 1, NA))
tmp <- tmp[, 2]
tmp<-bootCI(ovlp["Gr2","Gr8"], tmp)
boothigh["Gr2","Gr8"]<-tmp[4, 2]
bootlow["Gr2","Gr8"]<-tmp[4, 1]

tmp <- bootEst(gr2boot, gr12boot, adjust = c(NA, 1, NA))
tmp <- tmp[, 2]
tmp<-bootCI(ovlp["Gr2","Gr12"], tmp)
boothigh["Gr2","Gr12"]<-tmp[4, 2]
bootlow["Gr2","Gr12"]<-tmp[4, 1]

tmp <- bootEst(gr2boot, gr13boot, adjust = c(NA, 1, NA))
tmp <- tmp[, 2]
tmp<-bootCI(ovlp["Gr2","Gr13"], tmp)
boothigh["Gr2","Gr13"]<-tmp[4, 2]
bootlow["Gr2","Gr13"]<-tmp[4, 1]

tmp <- bootEst(gr2boot, gr22boot, adjust = c(NA, 1, NA))
tmp <- tmp[, 2]
tmp<-bootCI(ovlp["Gr2","Gr22"], tmp)
boothigh["Gr2","Gr22"]<-tmp[4, 2]
bootlow["Gr2","Gr22"]<-tmp[4, 1]

tmp <- bootEst(gr2boot, gr20boot, adjust = c(NA, 1, NA))
tmp <- tmp[, 2]
tmp<-bootCI(ovlp["Gr2","Gr20"], tmp)
boothigh["Gr2","Gr20"]<-tmp[4, 2]
bootlow["Gr2","Gr20"]<-tmp[4, 1]

tmp <- bootEst(gr4boot, gr8boot, adjust = c(NA, 1, NA))
tmp <- tmp[, 2]
tmp<-bootCI(ovlp["Gr4","Gr8"], tmp)
boothigh["Gr4","Gr8"]<-tmp[4, 2]
bootlow["Gr4","Gr8"]<-tmp[4, 1]

tmp <- bootEst(gr4boot, gr12boot, adjust = c(NA, 1, NA))
tmp <- tmp[, 2]
tmp<-bootCI(ovlp["Gr4","Gr12"], tmp)
boothigh["Gr4","Gr12"]<-tmp[4, 2]
bootlow["Gr4","Gr12"]<-tmp[4, 1]

tmp <- bootEst(gr4boot, gr13boot, adjust = c(NA, 1, NA))
tmp <- tmp[, 2]
tmp<-bootCI(ovlp["Gr4","Gr13"], tmp)
boothigh["Gr4","Gr13"]<-tmp[4, 2]
bootlow["Gr4","Gr13"]<-tmp[4, 1]

tmp <- bootEst(gr4boot, gr22boot, adjust = c(NA, 1, NA))
tmp <- tmp[, 2]
tmp<-bootCI(ovlp["Gr4","Gr22"], tmp)
boothigh["Gr4","Gr22"]<-tmp[4, 2]
bootlow["Gr4","Gr22"]<-tmp[4, 1]

tmp <- bootEst(gr4boot, gr20boot, adjust = c(NA, 1, NA))
tmp <- tmp[, 2]
tmp<-bootCI(ovlp["Gr4","Gr20"], tmp)
boothigh["Gr4","Gr20"]<-tmp[4, 2]
bootlow["Gr4","Gr20"]<-tmp[4, 1]

tmp <- bootEst(gr8boot, gr12boot, adjust = c(NA, 1, NA))
tmp <- tmp[, 2]
tmp<-bootCI(ovlp["Gr8","Gr12"], tmp)
boothigh["Gr8","Gr12"]<-tmp[4, 2]
bootlow["Gr8","Gr12"]<-tmp[4, 1]

tmp <- bootEst(gr8boot, gr13boot, adjust = c(NA, 1, NA))
tmp <- tmp[, 2]
tmp<-bootCI(ovlp["Gr8","Gr13"], tmp)
boothigh["Gr8","Gr13"]<-tmp[4, 2]
bootlow["Gr8","Gr13"]<-tmp[4, 1]

tmp <- bootEst(gr8boot, gr22boot, adjust = c(NA, 1, NA))
tmp <- tmp[, 2]
tmp<-bootCI(ovlp["Gr8","Gr22"], tmp)
boothigh["Gr8","Gr22"]<-tmp[4, 2]
bootlow["Gr8","Gr22"]<-tmp[4, 1]

tmp <- bootEst(gr8boot, gr20boot, adjust = c(NA, 1, NA))
tmp <- tmp[, 2]
tmp<-bootCI(ovlp["Gr8","Gr20"], tmp)
boothigh["Gr8","Gr20"]<-tmp[4, 2]
bootlow["Gr8","Gr20"]<-tmp[4, 1]

tmp <- bootEst(gr12boot, gr13boot, adjust = c(NA, 1, NA))
tmp <- tmp[, 2]
tmp<-bootCI(ovlp["Gr12","Gr13"], tmp)
boothigh["Gr12","Gr13"]<-tmp[4, 2]
bootlow["Gr12","Gr13"]<-tmp[4, 1]

tmp <- bootEst(gr12boot, gr22boot, adjust = c(NA, 1, NA))
tmp <- tmp[, 2]
tmp<-bootCI(ovlp["Gr12","Gr22"], tmp)
boothigh["Gr12","Gr22"]<-tmp[4, 2]
bootlow["Gr12","Gr22"]<-tmp[4, 1]

tmp <- bootEst(gr12boot, gr20boot, adjust = c(NA, 1, NA))
tmp <- tmp[, 2]
tmp<-bootCI(ovlp["Gr12","Gr20"], tmp)
boothigh["Gr12","Gr20"]<-tmp[4, 2]
bootlow["Gr12","Gr20"]<-tmp[4, 1]

tmp <- bootEst(gr13boot, gr22boot, adjust = c(NA, 1, NA))
tmp <- tmp[, 2]
tmp<-bootCI(ovlp["Gr13","Gr22"], tmp)
boothigh["Gr13","Gr22"]<-tmp[4, 2]
bootlow["Gr13","Gr22"]<-tmp[4, 1]

tmp <- bootEst(gr13boot, gr20boot, adjust = c(NA, 1, NA))
tmp <- tmp[, 2]
tmp<-bootCI(ovlp["Gr13","Gr20"], tmp)
boothigh["Gr13","Gr20"]<-tmp[4, 2]
bootlow["Gr13","Gr20"]<-tmp[4, 1]
###R environment saved here

#density plots
gr4plot<-densityPlot(gr4, xcenter="midnight", rug=F, ylim=c(0,0.7), col="#377eb8", main=NULL, axes=F, ann=F, xaxt='n', extend=NULL, lwd=1.7)
gr20plot<-densityPlot(gr20, xcenter="midnight", rug=F, ylim=c(0,0.7), col="#377eb8", main=NULL, axes=F, ann=F, xaxt='n', extend=NULL, lwd=1.7)

densityPlot(gr2, xcenter="midnight", rug=F, ylim=c(0,0.7), col=NA, main=NULL, axes=F, ann=F, xaxt='n', extend=NULL, lwd=1.7)
#grid(nx = 26, ny = NA, col = "lightgray", lty = "dotted",
     #lwd = par("lwd"), equilogs = TRUE)
polygon(gr4plot$x, pmin(gr4plot$y, gr20plot$y),80,border=NA,  col = 'lightgray', lwd=1.7)
par(new=TRUE)
densityPlot(gr2, xcenter="midnight", rug=F, ylim=c(0,0.7), col="#e41a1c", main=NULL, axes=F, ann=F, xaxt='n', extend=NULL, lwd=1.7, lty=3)
par(new=TRUE)
densityPlot(gr4, xcenter="midnight", rug=F, ylim=c(0,0.7), col="#377eb8", main=NULL, axes=F, ann=F, xaxt='n', extend=NULL, lwd=1.7, lty=3)
par(new=TRUE)
densityPlot(gr8, xcenter="midnight", rug=F, ylim=c(0,0.7), col="#4daf4a", main=NULL, axes=F, ann=F, xaxt='n', extend=NULL, lwd=1.7, lty=2)
par(new=TRUE)
densityPlot(gr12, xcenter="midnight", rug=F, ylim=c(0,0.7), col="#984ea3", main=NULL, axes=F, ann=F, xaxt='n', extend=NULL, lwd=1.7, lty=2)
par(new=TRUE)
densityPlot(gr13, xcenter="midnight", rug=F, ylim=c(0,0.7), col="#ff7f00", main=NULL, axes=F, ann=F, xaxt='n', extend=NULL, lwd=1.7, lty=2)
par(new=TRUE)
densityPlot(gr22, xcenter="midnight", rug=F, ylim=c(0,0.7), col="#c9b030", main=NULL, axes=F, ann=F, xaxt='n', extend=NULL, lwd=1.7)
par(new=TRUE)
densityPlot(gr20, xcenter="midnight", rug=F, ylim=c(0,0.7), col="#a65628", main=NULL, extend=NULL, lwd=1.7, ylab="Acoustic activity density", xaxt="n",
            cex.lab=1.3, cex.main=1.2, cex.sub=1.3)
legend('topright', c("Gr2", "Gr4", "Gr8", "Gr12", "Gr13","Gr22", "Gr20"), 
       lty=c(3,3,2,2,2,1,1), col=c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00",
       "#c9b030","#a65628"), bty='n', lwd=2.6)
axis(side=1, at=seq(-12, 12, by=2),tick=T , labels = c("12:00","14:00","16:00","18:00","20:00","22:00","00:00","2:00","4:00","6:00","8:00","10:00","12:00"))

