#PeerJ submission Gomez-Morales & Acevedo-Charry
rm(list=ls())

#Packages
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE)
    sapply(pkg, require, character.only = TRUE)
}

# Llamando los paquetes
packages <- c("ggplot2","reshape2","Rmisc","tidyr","progress")
ipak(packages)

#########
#Data
#########

data=read.csv("bandwidth.csv")
head(data)
data<-data[,c(1:5)]

data2=melt(as.data.frame(data),id=c("X","species"),
                  variable.name = "Frequency", value.name = "Value")
head(data2)

data2$group<-as.factor(paste(data2$species,data2$Frequency,sep = "_"))
with(data2, summary(group))

data2$nGroup<-as.numeric(factor(data2$group))
table(data2$nGroup)


#B1: gr12_dom_freq
R<-10000
B1<-rep(0,R);
data3<-subset(data2,nGroup==1)
for(kk in 1:R){
  boot.sample <- sample(data3$Value, replace = TRUE)
  B1[kk] <- mean(boot.sample)
  print(B1)
}
dB1<-c(Bmean=mean(B1),quantile(B1,c(0.025,0.975)),n=nrow(data3))
dB1

#B2: gr12_max_freq
B2<-rep(0,R);
data3<-subset(data2,nGroup==2)
for(kk in 1:R){
  boot.sample <- sample(data3$Value, replace = TRUE)
  B2[kk] <- mean(boot.sample)
  print(B2)
}
dB2<-c(Bmean=mean(B2),quantile(B2,c(0.025,0.975)),n=nrow(data3))
dB2

#B3: gr12_min_freq
B3<-rep(0,R);
data3<-subset(data2,nGroup==3)
for(kk in 1:R){
  boot.sample <- sample(data3$Value, replace = TRUE)
  B3[kk] <- mean(boot.sample)
  print(B3)
}
dB3<-c(Bmean=mean(B3),quantile(B3,c(0.025,0.975)),n=nrow(data3))
dB3

#B4: gr13_dom_freq
B4<-rep(0,R);
data3<-subset(data2,nGroup==4)
for(kk in 1:R){
  boot.sample <- sample(data3$Value, replace = TRUE)
  B4[kk] <- mean(boot.sample)
  print(B4)
}
dB4<-c(Bmean=mean(B4),quantile(B4,c(0.025,0.975)),n=nrow(data3))
dB4

#B5: gr13_max_freq
B5<-rep(0,R);
data3<-subset(data2,nGroup==5)
for(kk in 1:R){
  boot.sample <- sample(data3$Value, replace = TRUE)
  B5[kk] <- mean(boot.sample)
  print(B5)
}
dB5<-c(Bmean=mean(B5),quantile(B5,c(0.025,0.975)),n=nrow(data3))
dB5

#B6: gr13_min_freq
B6<-rep(0,R);
data3<-subset(data2,nGroup==6)
for(kk in 1:R){
  boot.sample <- sample(data3$Value, replace = TRUE)
  B6[kk] <- mean(boot.sample)
  print(B6)
}
dB6<-c(Bmean=mean(B6),quantile(B6,c(0.025,0.975)),n=nrow(data3))
dB6

#B7: gr2_dom_freq
B7<-rep(0,R);
data3<-subset(data2,nGroup==7)
for(kk in 1:R){
  boot.sample <- sample(data3$Value, replace = TRUE)
  B7[kk] <- mean(boot.sample)
  print(B7)
}
dB7<-c(Bmean=mean(B7),quantile(B7,c(0.025,0.975)),n=nrow(data3))
dB7

#B8: gr2_max_freq
B8<-rep(0,R);
data3<-subset(data2,nGroup==8)
for(kk in 1:R){
  boot.sample <- sample(data3$Value, replace = TRUE)
  B8[kk] <- mean(boot.sample)
  print(B8)
}
dB8<-c(Bmean=mean(B8),quantile(B8,c(0.025,0.975)),n=nrow(data3))
dB8

#B9: gr2_min_freq
B9<-rep(0,R);
data3<-subset(data2,nGroup==9)
for(kk in 1:R){
  boot.sample <- sample(data3$Value, replace = TRUE)
  B9[kk] <- mean(boot.sample)
  print(B9)
}
dB9<-c(Bmean=mean(B9),quantile(B9,c(0.025,0.975)),n=nrow(data3))
dB9

#B10: gr20_dom_freq
B10<-rep(0,R);
data3<-subset(data2,nGroup==10)
for(kk in 1:R){
  boot.sample <- sample(data3$Value, replace = TRUE)
  B10[kk] <- mean(boot.sample)
  print(B10)
}
dB10<-c(Bmean=mean(B10),quantile(B10,c(0.025,0.975)),n=nrow(data3))
dB10

#B11: gr20_max_freq
B11<-rep(0,R);
data3<-subset(data2,nGroup==11)
for(kk in 1:R){
  boot.sample <- sample(data3$Value, replace = TRUE)
  B11[kk] <- mean(boot.sample)
  print(B11)
}
dB11<-c(Bmean=mean(B11),quantile(B11,c(0.025,0.975)),n=nrow(data3))
dB11

#B12: gr20_min_freq
B12<-rep(0,R);
data3<-subset(data2,nGroup==12)
for(kk in 1:R){
  boot.sample <- sample(data3$Value, replace = TRUE)
  B12[kk] <- mean(boot.sample)
  print(B12)
}
dB12<-c(Bmean=mean(B12),quantile(B12,c(0.025,0.975)),n=nrow(data3))
dB12

#B13: gr22_dom_freq
B13<-rep(0,R);
data3<-subset(data2,nGroup==13)
for(kk in 1:R){
  boot.sample <- sample(data3$Value, replace = TRUE)
  B13[kk] <- mean(boot.sample)
  print(B13)
}
dB13<-c(Bmean=mean(B13),quantile(B13,c(0.025,0.975)),n=nrow(data3))
dB13

#B14: gr22_max_freq
B14<-rep(0,R);
data3<-subset(data2,nGroup==14)
for(kk in 1:R){
  boot.sample <- sample(data3$Value, replace = TRUE)
  B14[kk] <- mean(boot.sample)
  print(B14)
}
dB14<-c(Bmean=mean(B14),quantile(B14,c(0.025,0.975)),n=nrow(data3))
dB14

#B15: gr22_min_freq
B15<-rep(0,R);
data3<-subset(data2,nGroup==15)
for(kk in 1:R){
  boot.sample <- sample(data3$Value, replace = TRUE)
  B15[kk] <- mean(boot.sample)
  print(B15)
}
dB15<-c(Bmean=mean(B15),quantile(B15,c(0.025,0.975)),n=nrow(data3))
dB15

#B16: gr4_dom_freq
B16<-rep(0,R);
data3<-subset(data2,nGroup==16)
for(kk in 1:R){
  boot.sample <- sample(data3$Value, replace = TRUE)
  B16[kk] <- mean(boot.sample)
  print(B16)
}
dB16<-c(Bmean=mean(B16),quantile(B16,c(0.025,0.975)),n=nrow(data3))
dB16

#B17: gr4_max_freq
B17<-rep(0,R);
data3<-subset(data2,nGroup==17)
for(kk in 1:R){
  boot.sample <- sample(data3$Value, replace = TRUE)
  B17[kk] <- mean(boot.sample)
  print(B17)
}
dB17<-c(Bmean=mean(B17),quantile(B17,c(0.025,0.975)),n=nrow(data3))
dB17

#B18: gr4_min_freq
B18<-rep(0,R);
data3<-subset(data2,nGroup==18)
for(kk in 1:R){
  boot.sample <- sample(data3$Value, replace = TRUE)
  B18[kk] <- mean(boot.sample)
  print(B18)
}
dB18<-c(Bmean=mean(B18),quantile(B18,c(0.025,0.975)),n=nrow(data3))
dB18

#B19: gr8_dom_freq
B19<-rep(0,R);
data3<-subset(data2,nGroup==19)
for(kk in 1:R){
  boot.sample <- sample(data3$Value, replace = TRUE)
  B19[kk] <- mean(boot.sample)
  print(B19)
}
dB19<-c(Bmean=mean(B19),quantile(B19,c(0.025,0.975)),n=nrow(data3))
dB19

#B20: gr8_max_freq
B20<-rep(0,R);
data3<-subset(data2,nGroup==20)
for(kk in 1:R){
  boot.sample <- sample(data3$Value, replace = TRUE)
  B20[kk] <- mean(boot.sample)
  print(B20)
}
dB20<-c(Bmean=mean(B20),quantile(B20,c(0.025,0.975)),n=nrow(data3))
dB20

#B21: gr8_min_freq
B21<-rep(0,R);
data3<-subset(data2,nGroup==21)
for(kk in 1:R){
  boot.sample <- sample(data3$Value, replace = TRUE)
  B21[kk] <- mean(boot.sample)
  print(B21)
}
dB21<-c(Bmean=mean(B21),quantile(B21,c(0.025,0.975)),n=nrow(data3))
dB21

#Join results
#Resume data of Bootstrap (Bootstrap mean, confidence limits[2.5%-97.5%],n)
dbR=data.frame(dB1,dB2,dB3,dB4,dB5,dB6,
               dB7,dB8,dB9,dB10,dB11,dB12,
               dB13,dB14,dB15,dB16,dB17,dB18,
               dB19,dB20,dB21)
t.dbR<-t(dbR)
datboots<-as.data.frame(t.dbR)
head(datboots)
datboots$Group<-c("gr12_dom_freq", "gr12_max_freq", "gr12_min_freq",
                  "gr13_dom_freq", "gr13_max_freq", "gr13_min_freq",
                  "gr2_dom_freq",  "gr2_max_freq",  "gr2_min_freq",
                  "gr20_dom_freq", "gr20_max_freq", "gr20_min_freq",
                  "gr22_dom_freq", "gr22_max_freq", "gr22_min_freq",
                   "gr4_dom_freq",  "gr4_max_freq",  "gr4_min_freq",
                  "gr8_dom_freq",  "gr8_max_freq", "gr8_min_freq")
datboots<-datboots %>% 
  separate(Group, c("Species","Frequency","freq"),"_")
head(datboots)

colnames(datboots)<-c("Bmean","Lower","Upper","n","Species","Frequency","freq")

write.csv(datboots,"BootstrappedBandwidthF.csv")

#Call the file "BootstrappedBandwidthF.csv"####
datboots=read.csv("BootstrappedBandwidthF.csv")

#Adjust terminology to the figure
datboots = within(datboots, Frequency  <- ifelse (Frequency  =="dom", "Dominant",
                             ifelse (Frequency  =="max", "Maximum",
                             ifelse (Frequency  =="min", "Minimum",NA))))

datboots = within(datboots, Species  <- ifelse (Species  =="gr2", "Gr2",
                             ifelse (Species  =="gr4", "Gr4",
                             ifelse (Species  =="gr8", "Gr8",
                             ifelse (Species  =="gr12", "Gr12",
                             ifelse (Species  =="gr13", "Gr13",
                             ifelse (Species  =="gr22", "Gr22",
                             ifelse (Species  =="gr20", "Gr20",NA))))))))

#Species in order
datboots$Species <- factor(datboots$Species,
                           levels=c("Gr2", "Gr4", "Gr8", "Gr12", "Gr13","Gr22", "Gr20"))
#Frequencies in order
datboots$Frequency <- factor(datboots$Frequency,
                           levels=c("Minimum","Dominant","Maximum"))

datboots2<-spread(datboots[,c(2,6,7)],Frequency,Bmean)

ggplot(datboots,aes(x=Species, fill=Species))+
  geom_linerange(data=datboots2,
                 aes(color=Species,ymin= Minimum/1000, ymax = Maximum/1000))+
  geom_point(aes(y=Bmean/1000,shape=Frequency), size=2.5)+
  scale_fill_manual(values=c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#c9b030","#a65628"))+
  scale_color_manual(values=c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#c9b030","#a65628"))+
  scale_shape_manual(values=c(25,22,24))+
  guides(fill="none",color="none")+
  labs(x="\nSpecies",y="Frequency (kHz)\n")+
  theme_bw()+
  theme(legend.position = c(0.2,0.8),
        legend.box.background = element_rect(colour = "black"),
        strip.background = element_blank())
