library(dplyr)
library(ggplot2)

data<-data.frame(Hours = c(4.1,2.8,9.2,10.8,8.2,11.7,11.3),
           Dom = c(2.604,3.532,6.752,13.505,16.371,20.340,20.776),
           upp = c(2.620,3.551,6.767,13.551,16.540,20.441,21.442),
           low = c(2.589,3.513,6.742,13.467,16.265,20.170,20.134),
           species = c("Gr2","Gr4", "Gr8", "Gr12", "Gr13", "Gr22", "Gr20"))

data$species <- factor(data$species,
                           levels=c("Gr2", "Gr4", "Gr8", "Gr12", "Gr13","Gr22", "Gr20"))

data %>% ggplot()+
  geom_errorbar(aes(x=Hours, ymin=upp, ymax=low, color = species), width=0.2, size=0.5)+
  geom_point(aes(x = Hours, y = Dom, color = species))+
   geom_smooth(method= "glm", aes(x = Hours, y = Dom), color = "darkgray")+
  geom_text(aes(x = Hours, y = Dom, label = species), size=3.5, 
            hjust=ifelse(data$species=="Gr22",0.4,0.6),
            vjust=ifelse(data$species=="Gr22",1,-0.5))+
  scale_color_manual(values = c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00",
                                "#c9b030","#a65628"))+
  labs( x ="Acoustic activity span (h)",
        y = "Average dominant frequency (kHz)"
  )+theme_bw()+
  theme(legend.position = "none")+
scale_x_continuous(breaks = c(2,4,6,8,10,12))
