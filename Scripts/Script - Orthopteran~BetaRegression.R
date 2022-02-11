#PeerJ submission Gomez-Morales & Acevedo-Charry
rm(list=ls())

#########
#Data
#########

data=read.csv("factors.csv")
names(data)
#this data include many thinks, but of interest:
  #date of record[1] as the events, 
  #number of detections for seven orthopterans [2:8],
  #precipitation value [9],
  #Fraction of the moon bright [10],
  #Minimum temperature day in Celsius [12],
  #Maximum temperature day in Celsius [13]
summary(data)

#Figure

library(reshape2)
data2<-data[c(1:10,12,13)]

names(data2)
figure<-melt(as.data.frame(data2),id=c("date","precip","fraction","MinC","MaxC"))
head(figure)
colnames(figure)=c("Date", "Precipitation", "MoonFraction",
               "TemperatureMin","TemperatureMax", "Species","Detections")

#The proportion of detections per day
figure$Detections<-figure$Detections/48 

#to avoid zero inflated in some species
figure$Detections<-(figure$Detections*(61-1)+0.5)/61 

summary(figure)

toGraph<-melt(as.data.frame(figure),id=c("Date","Species","Detections"))
head(toGraph)

#Species in order
toGraph$Species <- factor(toGraph$Species,
                          levels=c("Gr2", "Gr4", "Gr8", "Gr12", "Gr13","Gr22", "Gr20"))

#Change the name to labeling
toGraph = within(toGraph, variable  <- ifelse (variable  =="MoonFraction", "Moon (fraction)",
                             ifelse (variable  =="Precipitation", "Precipitation (mm)",
                             ifelse (variable  =="TemperatureMin", "MinTem (ºC)",
                             ifelse (variable  =="TemperatureMax", "MaxTem (ºC)",NA)))))

#Variables in order
toGraph$variable <- factor(toGraph$variable,
                           levels=c("Moon (fraction)", "Precipitation (mm)", "MinTem (ºC)", "MaxTem (ºC)"))

library(ggplot2)
ggplot(toGraph,aes(value,Detections,color=Species))+
  geom_point(alpha=0.4)+
  geom_smooth(method = "glm", 
    method.args = list(family = "binomial"), 
    se = F)+
  scale_color_manual(values=c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#c9b030","#a65628"))+
  labs(x="\nEnvironmental factor value",y="Proportion of recordings with detections\n")+
  theme_bw()+
  theme(legend.position = "none",
        strip.background = element_blank())+
   facet_grid(Species~variable, scales = "free")

#Only the variable selected in the models
Gr2<-subset(toGraph,Species=="Gr2"&variable=="Precipitation (mm)")
Gr4<-subset(toGraph,Species=="Gr4"&variable=="Precipitation (mm)")
Gr13<-subset(toGraph,Species=="Gr13"&variable=="Moon (fraction)")
Gr22<-subset(toGraph,Species=="Gr22"&variable=="Moon (fraction)")

t1<-c("Precipitation (mm)","MinTem (ºC)","Moon (fraction)")
t2<-c("MinTem (ºC)","Moon (fraction)")
t3<-c("Precipitation (mm)","MaxTem (ºC)","Moon (fraction)")

Gr8<-subset(toGraph,Species=="Gr8"&variable==t1)
Gr12<-subset(toGraph,Species=="Gr12"&variable==t2)
Gr20<-subset(toGraph,Species=="Gr20" & variable==t3)

toGraphReg<-rbind(Gr2,Gr4,Gr13,Gr22,Gr8,Gr12,Gr20)

ggplot(toGraph,aes(value,Detections,color=Species))+
  geom_point(alpha=0.4)+
  geom_smooth(method = "glm", 
    method.args = list(family = "binomial"), 
    se = F, data=toGraphReg,fullrange=T)+
  scale_color_manual(values=c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00","#c9b030","#a65628"))+
  labs(x="\nEnvironmental factor value",y="Proportion of recordings with detections\n")+
  theme_bw()+
  theme(legend.position = "none",
        strip.background = element_blank())+
   facet_grid(Species~variable, scales = "free")


#Centering variables around the mean and dividing by two standard deviations

library(dplyr) #data manipulation, centering and scaling

dt<-data%>%
  mutate_each_(funs(scale(.) %>%
                      as.vector),
							vars=c("precip","fraction","MinC","MaxC"))
summary(dt)
dt<-dt[c(1:10,12,13)]

library(reshape2)
names(dt)
df<-melt(as.data.frame(dt),id=c("date","precip","fraction","MinC","MaxC"))
head(df)
colnames(df)=c("Date", "Precipitation", "MoonFraction",
               "TemperatureMin","TemperatureMax", "Species","Detections")

#The proportion of detections per day
df$Detections<-df$Detections/48 

#to avoid zero inflated in some species
df$Detections<-(df$Detections*(61-1)+0.5)/61 

summary(df)

library(ggplot2)

ggplot(df,aes(x=Species,y=Detections))+
   geom_violin()+
   geom_jitter(color="#2980B9",size=2,shape=1,width = 0.15)+
   theme_bw()


#################################
#Modeling by species (Beta distribution)
#################################

#Sp1 = Gr2####
head(df)

sp1<-subset(df,Species=="Gr2") 

#Model selection AICc
m.AICc <- function(modelos,data,n){
  LL <- sapply(modelos,logLik)
  k <- sapply(lapply(modelos,logLik),attr,"df")
  AIC <- -2*LL+2*k
  AICc <- AIC+((2*k*(k+1))/(n-k-1))
  d.AICc <- AICc-min(AICc)
  w.AICc <- (exp(-0.5*d.AICc))/sum(exp(-0.5*d.AICc))
  data<-data
  data.frame(n.par=k,log.lik=LL,AICc=AICc,AIC=AIC,delta.AICc=d.AICc,w.AICc=w.AICc,name=data,row.names=names(LL))[order(d.AICc),]
}

library(plyr)
## Run models via bootstrap
top_ranked<-list()
null_model_AICc<-list()
null_model_position<-list()
#coef<-list()
#r2<-list()
library(betareg)
global.sp1<-betareg(Detections~1+Precipitation+MoonFraction+TemperatureMin+TemperatureMax,
                      data=sp1)

for (i in 1:10000){
## Sample 50 random days per species
sp1b<-sample_n(sp1,50)

m1<-betareg(Detections~1, data=sp1b)
m2<-betareg(Detections~1+Precipitation,data=sp1b)
m3<-betareg(Detections~1+MoonFraction,data=sp1b)
m4<-betareg(Detections~1+TemperatureMin,data=sp1b)
m5<-betareg(Detections~1+TemperatureMax,data=sp1b)

m6<-betareg(Detections~1+Precipitation+MoonFraction,data=sp1b)
m7<-betareg(Detections~1+Precipitation+TemperatureMin,data=sp1b)
m8<-betareg(Detections~1+Precipitation+TemperatureMax,data=sp1b)
m9<-betareg(Detections~1+MoonFraction+TemperatureMin,data=sp1b)
m10<-betareg(Detections~1+MoonFraction+TemperatureMax,data=sp1b)
m11<-betareg(Detections~1+TemperatureMin+TemperatureMax,data=sp1b)

m12<-betareg(Detections~1+Precipitation+MoonFraction+TemperatureMin,data=sp1b)
m13<-betareg(Detections~1+Precipitation+MoonFraction+TemperatureMax,data=sp1b)
m14<-betareg(Detections~1+Precipitation+TemperatureMin+TemperatureMax,data=sp1b)
m15<-betareg(Detections~1+MoonFraction+TemperatureMin+TemperatureMax,data=sp1b)

m16<-betareg(Detections~1+Precipitation+MoonFraction+TemperatureMin+TemperatureMax,
                      data=sp1b)

ranks<-m.AICc(list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16),
					as.integer(c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16")),
					length(sp1b$Detections))

top_ranked[[i]]<-ranks[1,]
null_model_AICc[[i]]<-ranks[ranks$name=="1",]
null_model_position[[i]]<-which(ranks$name=="1",)

print(i)
}

  ## Transforming lists in data.frames
df_top_ranked<-data.frame(matrix(unlist(top_ranked), nrow=10000, byrow=T))
df_null_model_AICc<-data.frame(matrix(unlist(null_model_AICc), nrow=10000, byrow=T))
df_null_model_position<-data.frame(matrix(unlist(null_model_position), nrow=10000, byrow=T))

## Top-model frequence
plausible_mod_frequency<-as.data.frame(ftable(df_top_ranked[,7]))
plausible_mod_frequency
#m2 (76.2%)
wi<-subset(df_top_ranked, X7=="2")
wi_mean<-mean(wi$X6)
wi_mean #0.30

#m2 (betareg(Detections~1+Precipitation)
sp1m2<-betareg(Detections~1+Precipitation,data=sp1)
summary(sp1m2)

#Sp2 = Gr4####
head(df)

sp2<-subset(df,Species=="Gr4") 

## Run models via bootstrap
top_ranked<-list()
null_model_AICc<-list()
null_model_position<-list()
#coef<-list()
#r2<-list()
global.sp2<-betareg(Detections~1+Precipitation+MoonFraction+TemperatureMin+TemperatureMax,
                      data=sp2)

for (i in 1:10000){
## Sample 50 random days per species
sp2b<-sample_n(sp2,50)

m1<-betareg(Detections~1, data=sp2b)
m2<-betareg(Detections~1+Precipitation,data=sp2b)
m3<-betareg(Detections~1+MoonFraction,data=sp2b)
m4<-betareg(Detections~1+TemperatureMin,data=sp2b)
m5<-betareg(Detections~1+TemperatureMax,data=sp2b)

m6<-betareg(Detections~1+Precipitation+MoonFraction,data=sp2b)
m7<-betareg(Detections~1+Precipitation+TemperatureMin,data=sp2b)
m8<-betareg(Detections~1+Precipitation+TemperatureMax,data=sp2b)
m9<-betareg(Detections~1+MoonFraction+TemperatureMin,data=sp2b)
m10<-betareg(Detections~1+MoonFraction+TemperatureMax,data=sp2b)
m11<-betareg(Detections~1+TemperatureMin+TemperatureMax,data=sp2b)

m12<-betareg(Detections~1+Precipitation+MoonFraction+TemperatureMin,data=sp2b)
m13<-betareg(Detections~1+Precipitation+MoonFraction+TemperatureMax,data=sp2b)
m14<-betareg(Detections~1+Precipitation+TemperatureMin+TemperatureMax,data=sp2b)
m15<-betareg(Detections~1+MoonFraction+TemperatureMin+TemperatureMax,data=sp2b)

m16<-betareg(Detections~1+Precipitation+MoonFraction+TemperatureMin+TemperatureMax,
                      data=sp2b)

ranks<-m.AICc(list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16),
					as.integer(c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16")),
					length(sp2b$Detections))

top_ranked[[i]]<-ranks[1,]
null_model_AICc[[i]]<-ranks[ranks$name=="1",]
null_model_position[[i]]<-which(ranks$name=="1",)

print(i)
}

## Transforming lists in data.frames
df_top_ranked<-data.frame(matrix(unlist(top_ranked), nrow=10000, byrow=T))
df_null_model_AICc<-data.frame(matrix(unlist(null_model_AICc), nrow=10000, byrow=T))
df_null_model_position<-data.frame(matrix(unlist(null_model_position), nrow=10000, byrow=T))

## Top-model frequence
plausible_mod_frequency<-as.data.frame(ftable(df_top_ranked[,7]))
plausible_mod_frequency
#m2 (94.81%)
wi<-subset(df_top_ranked, X7=="2")
wi_mean<-mean(wi$X6)
wi_mean #0.3489597

#m2 (betareg(Detections~1+Precipitation)
sp2m2<-betareg(Detections~1+Precipitation,data=sp2)
summary(sp2m2)

#Sp3 = Gr8####
head(df)

sp3<-subset(df,Species=="Gr8") 

## Run models via bootstrap
top_ranked<-list()
null_model_AICc<-list()
null_model_position<-list()
#coef<-list()
#r2<-list()
global.sp3<-betareg(Detections~1+Precipitation+MoonFraction+TemperatureMin+TemperatureMax,
                      data=sp3)

for (i in 1:10000){
## Sample 50 random days per species
sp3b<-sample_n(sp3,50)

m1<-betareg(Detections~1, data=sp3b)
m2<-betareg(Detections~1+Precipitation,data=sp3b)
m3<-betareg(Detections~1+MoonFraction,data=sp3b)
m4<-betareg(Detections~1+TemperatureMin,data=sp3b)
m5<-betareg(Detections~1+TemperatureMax,data=sp3b)

m6<-betareg(Detections~1+Precipitation+MoonFraction,data=sp3b)
m7<-betareg(Detections~1+Precipitation+TemperatureMin,data=sp3b)
m8<-betareg(Detections~1+Precipitation+TemperatureMax,data=sp3b)
m9<-betareg(Detections~1+MoonFraction+TemperatureMin,data=sp3b)
m10<-betareg(Detections~1+MoonFraction+TemperatureMax,data=sp3b)
m11<-betareg(Detections~1+TemperatureMin+TemperatureMax,data=sp3b)

m12<-betareg(Detections~1+Precipitation+MoonFraction+TemperatureMin,data=sp3b)
m13<-betareg(Detections~1+Precipitation+MoonFraction+TemperatureMax,data=sp3b)
m14<-betareg(Detections~1+Precipitation+TemperatureMin+TemperatureMax,data=sp3b)
m15<-betareg(Detections~1+MoonFraction+TemperatureMin+TemperatureMax,data=sp3b)

m16<-betareg(Detections~1+Precipitation+MoonFraction+TemperatureMin+TemperatureMax,
                      data=sp3b)

ranks<-m.AICc(list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16),
					as.integer(c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16")),
					length(sp3b$Detections))

top_ranked[[i]]<-ranks[1,]
null_model_AICc[[i]]<-ranks[ranks$name=="1",]
null_model_position[[i]]<-which(ranks$name=="1",)

print(i)
}

## Transforming lists in data.frames
df_top_ranked<-data.frame(matrix(unlist(top_ranked), nrow=10000, byrow=T))
df_null_model_AICc<-data.frame(matrix(unlist(null_model_AICc), nrow=10000, byrow=T))
df_null_model_position<-data.frame(matrix(unlist(null_model_position), nrow=10000, byrow=T))

## Top-model frequence
plausible_mod_frequency<-as.data.frame(ftable(df_top_ranked[,7]))
plausible_mod_frequency
#m12 (82.39%)
wi<-subset(df_top_ranked, X7=="12")
wi_mean<-mean(wi$X6)
wi_mean #0.421931

#m12 (betareg(Detections~1+Precipitation+MoonFraction+TemperatureMin)
sp3m2<-betareg(Detections~1+Precipitation+MoonFraction+TemperatureMin,data=sp3)
summary(sp3m2)

#Sp4 = Gr12####
head(df)

sp4<-subset(df,Species=="Gr12") 

## Run models via bootstrap
top_ranked<-list()
null_model_AICc<-list()
null_model_position<-list()
#coef<-list()
#r2<-list()
global.sp4<-betareg(Detections~1+Precipitation+MoonFraction+TemperatureMin+TemperatureMax,
                      data=sp4)

for (i in 1:10000){
## Sample 50 random days per species
sp4b<-sample_n(sp4,50)

m1<-betareg(Detections~1, data=sp4b)
m2<-betareg(Detections~1+Precipitation,data=sp4b)
m3<-betareg(Detections~1+MoonFraction,data=sp4b)
m4<-betareg(Detections~1+TemperatureMin,data=sp4b)
m5<-betareg(Detections~1+TemperatureMax,data=sp4b)

m6<-betareg(Detections~1+Precipitation+MoonFraction,data=sp4b)
m7<-betareg(Detections~1+Precipitation+TemperatureMin,data=sp4b)
m8<-betareg(Detections~1+Precipitation+TemperatureMax,data=sp4b)
m9<-betareg(Detections~1+MoonFraction+TemperatureMin,data=sp4b)
m10<-betareg(Detections~1+MoonFraction+TemperatureMax,data=sp4b)
m11<-betareg(Detections~1+TemperatureMin+TemperatureMax,data=sp4b)

m12<-betareg(Detections~1+Precipitation+MoonFraction+TemperatureMin,data=sp4b)
m13<-betareg(Detections~1+Precipitation+MoonFraction+TemperatureMax,data=sp4b)
m14<-betareg(Detections~1+Precipitation+TemperatureMin+TemperatureMax,data=sp4b)
m15<-betareg(Detections~1+MoonFraction+TemperatureMin+TemperatureMax,data=sp4b)

m16<-betareg(Detections~1+Precipitation+MoonFraction+TemperatureMin+TemperatureMax,
                      data=sp4b)

ranks<-m.AICc(list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16),
					as.integer(c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16")),
					length(sp4b$Detections))

top_ranked[[i]]<-ranks[1,]
null_model_AICc[[i]]<-ranks[ranks$name=="1",]
null_model_position[[i]]<-which(ranks$name=="1",)

print(i)
}

## Transforming lists in data.frames
df_top_ranked<-data.frame(matrix(unlist(top_ranked), nrow=10000, byrow=T))
df_null_model_AICc<-data.frame(matrix(unlist(null_model_AICc), nrow=10000, byrow=T))
df_null_model_position<-data.frame(matrix(unlist(null_model_position), nrow=10000, byrow=T))

## Top-model frequence
plausible_mod_frequency<-as.data.frame(ftable(df_top_ranked[,7]))
plausible_mod_frequency
#m9
wi<-subset(df_top_ranked, X7=="9")
wi_mean<-mean(wi$X6)
wi_mean

#m2 (betareg(Detections~1+MoonFraction+TemperatureMin,data=sp4)
sp4m2<-betareg(Detections~1+MoonFraction+TemperatureMin,data=sp4)
summary(sp4m2)


#Sp5 = Gr13####
head(df)

sp5<-subset(df,Species=="Gr13") 

## Run models via bootstrap
top_ranked<-list()
null_model_AICc<-list()
null_model_position<-list()
#coef<-list()
#r2<-list()
global.sp5<-betareg(Detections~1+Precipitation+MoonFraction+TemperatureMin+TemperatureMax,
                      data=sp5)

for (i in 1:10000){
## Sample 50 random days per species
sp5b<-sample_n(sp5,50)

m1<-betareg(Detections~1, data=sp5b)
m2<-betareg(Detections~1+Precipitation,data=sp5b)
m3<-betareg(Detections~1+MoonFraction,data=sp5b)
m4<-betareg(Detections~1+TemperatureMin,data=sp5b)
m5<-betareg(Detections~1+TemperatureMax,data=sp5b)

m6<-betareg(Detections~1+Precipitation+MoonFraction,data=sp5b)
m7<-betareg(Detections~1+Precipitation+TemperatureMin,data=sp5b)
m8<-betareg(Detections~1+Precipitation+TemperatureMax,data=sp5b)
m9<-betareg(Detections~1+MoonFraction+TemperatureMin,data=sp5b)
m10<-betareg(Detections~1+MoonFraction+TemperatureMax,data=sp5b)
m11<-betareg(Detections~1+TemperatureMin+TemperatureMax,data=sp5b)

m12<-betareg(Detections~1+Precipitation+MoonFraction+TemperatureMin,data=sp5b)
m13<-betareg(Detections~1+Precipitation+MoonFraction+TemperatureMax,data=sp5b)
m14<-betareg(Detections~1+Precipitation+TemperatureMin+TemperatureMax,data=sp5b)
m15<-betareg(Detections~1+MoonFraction+TemperatureMin+TemperatureMax,data=sp5b)

m16<-betareg(Detections~1+Precipitation+MoonFraction+TemperatureMin+TemperatureMax,
                      data=sp5b)

ranks<-m.AICc(list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16),
					as.integer(c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16")),
					length(sp5b$Detections))

top_ranked[[i]]<-ranks[1,]
null_model_AICc[[i]]<-ranks[ranks$name=="1",]
null_model_position[[i]]<-which(ranks$name=="1",)

print(i)
}

## Transforming lists in data.frames
df_top_ranked<-data.frame(matrix(unlist(top_ranked), nrow=10000, byrow=T))
df_null_model_AICc<-data.frame(matrix(unlist(null_model_AICc), nrow=10000, byrow=T))
df_null_model_position<-data.frame(matrix(unlist(null_model_position), nrow=10000, byrow=T))

## Top-model frequence
plausible_mod_frequency<-as.data.frame(ftable(df_top_ranked[,7]))
plausible_mod_frequency
#m3 (56.81%)
wi<-subset(df_top_ranked, X7=="3")
wi_mean<-mean(wi$X6)
wi_mean

#m3 (betareg(Detections~1+MoonFraction,data=sp5)
sp5m3<-betareg(Detections~1+MoonFraction,data=sp5)
summary(sp5m3)

#Sp6 = Gr20####
head(df)

sp6<-subset(df,Species=="Gr20") 

## Run models via bootstrap
top_ranked<-list()
null_model_AICc<-list()
null_model_position<-list()
#coef<-list()
#r2<-list()
global.sp6<-betareg(Detections~1+Precipitation+MoonFraction+TemperatureMin+TemperatureMax,
                      data=sp6)

for (i in 1:10000){
## Sample 50 random days per species
sp6b<-sample_n(sp6,50)

m1<-betareg(Detections~1, data=sp6b)
m2<-betareg(Detections~1+Precipitation,data=sp6b)
m3<-betareg(Detections~1+MoonFraction,data=sp6b)
m4<-betareg(Detections~1+TemperatureMin,data=sp6b)
m5<-betareg(Detections~1+TemperatureMax,data=sp6b)

m6<-betareg(Detections~1+Precipitation+MoonFraction,data=sp6b)
m7<-betareg(Detections~1+Precipitation+TemperatureMin,data=sp6b)
m8<-betareg(Detections~1+Precipitation+TemperatureMax,data=sp6b)
m9<-betareg(Detections~1+MoonFraction+TemperatureMin,data=sp6b)
m10<-betareg(Detections~1+MoonFraction+TemperatureMax,data=sp6b)
m11<-betareg(Detections~1+TemperatureMin+TemperatureMax,data=sp6b)

m12<-betareg(Detections~1+Precipitation+MoonFraction+TemperatureMin,data=sp6b)
m13<-betareg(Detections~1+Precipitation+MoonFraction+TemperatureMax,data=sp6b)
m14<-betareg(Detections~1+Precipitation+TemperatureMin+TemperatureMax,data=sp6b)
m15<-betareg(Detections~1+MoonFraction+TemperatureMin+TemperatureMax,data=sp6b)

m16<-betareg(Detections~1+Precipitation+MoonFraction+TemperatureMin+TemperatureMax,
                      data=sp6b)

ranks<-m.AICc(list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16),
					as.integer(c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16")),
					length(sp6b$Detections))

top_ranked[[i]]<-ranks[1,]
null_model_AICc[[i]]<-ranks[ranks$name=="1",]
null_model_position[[i]]<-which(ranks$name=="1",)

print(i)
}

## Transforming lists in data.frames
df_top_ranked<-data.frame(matrix(unlist(top_ranked), nrow=10000, byrow=T))
df_null_model_AICc<-data.frame(matrix(unlist(null_model_AICc), nrow=10000, byrow=T))
df_null_model_position<-data.frame(matrix(unlist(null_model_position), nrow=10000, byrow=T))

## Top-model frequence
plausible_mod_frequency<-as.data.frame(ftable(df_top_ranked[,7]))
plausible_mod_frequency
#m13
wi<-subset(df_top_ranked, X7=="13")
wi_mean<-mean(wi$X6)
wi_mean

#m2 (betareg(Detections~1+Precipitation+MoonFraction+TemperatureMax,data=sp6b)
sp6m2<-betareg(Detections~1+Precipitation+MoonFraction+TemperatureMax,data=sp6)
summary(sp6m2)


#Sp7 = Gr22####
head(df)

sp7<-subset(df,Species=="Gr22") 

## Run models via bootstrap
top_ranked<-list()
null_model_AICc<-list()
null_model_position<-list()
#coef<-list()
#r2<-list()
global.sp7<-betareg(Detections~1+Precipitation+MoonFraction+TemperatureMin+TemperatureMax,
                      data=sp7)

for (i in 1:10000){
## Sample 50 random days per species
sp7b<-sample_n(sp7,50)

m1<-betareg(Detections~1, data=sp7b)
m2<-betareg(Detections~1+Precipitation,data=sp7b)
m3<-betareg(Detections~1+MoonFraction,data=sp7b)
m4<-betareg(Detections~1+TemperatureMin,data=sp7b)
m5<-betareg(Detections~1+TemperatureMax,data=sp7b)

m6<-betareg(Detections~1+Precipitation+MoonFraction,data=sp7b)
m7<-betareg(Detections~1+Precipitation+TemperatureMin,data=sp7b)
m8<-betareg(Detections~1+Precipitation+TemperatureMax,data=sp7b)
m9<-betareg(Detections~1+MoonFraction+TemperatureMin,data=sp7b)
m10<-betareg(Detections~1+MoonFraction+TemperatureMax,data=sp7b)
m11<-betareg(Detections~1+TemperatureMin+TemperatureMax,data=sp7b)

m12<-betareg(Detections~1+Precipitation+MoonFraction+TemperatureMin,data=sp7b)
m13<-betareg(Detections~1+Precipitation+MoonFraction+TemperatureMax,data=sp7b)
m14<-betareg(Detections~1+Precipitation+TemperatureMin+TemperatureMax,data=sp7b)
m15<-betareg(Detections~1+MoonFraction+TemperatureMin+TemperatureMax,data=sp7b)

m16<-betareg(Detections~1+Precipitation+MoonFraction+TemperatureMin+TemperatureMax,
                      data=sp7b)

ranks<-m.AICc(list(m1,m2,m3,m4,m5,m6,m7,m8,m9,m10,m11,m12,m13,m14,m15,m16),
					as.integer(c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16")),
					length(sp7b$Detections))

top_ranked[[i]]<-ranks[1,]
null_model_AICc[[i]]<-ranks[ranks$name=="1",]
null_model_position[[i]]<-which(ranks$name=="1",)

print(i)
}

## Transforming lists in data.frames
df_top_ranked<-data.frame(matrix(unlist(top_ranked), nrow=10000, byrow=T))
df_null_model_AICc<-data.frame(matrix(unlist(null_model_AICc), nrow=10000, byrow=T))
df_null_model_position<-data.frame(matrix(unlist(null_model_position), nrow=10000, byrow=T))

## Top-model frequence
plausible_mod_frequency<-as.data.frame(ftable(df_top_ranked[,7]))
plausible_mod_frequency
#m3
wi<-subset(df_top_ranked, X7=="3")
wi_mean<-mean(wi$X6)
wi_mean

#m3 (betareg(Detections~1+MoonFraction)
sp7m3<-betareg(Detections~1+MoonFraction,data=sp7)
summary(sp7m3)
