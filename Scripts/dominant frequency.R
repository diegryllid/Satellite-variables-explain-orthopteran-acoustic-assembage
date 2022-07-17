library(dplyr)
library(ggplot2)

data<-data.frame(Hours = c(4.1,2.8,9.2,10.8,8.2,11.7,11.3),
                 Dom = c(2.742,3.597,8.46,13.606,14.797,18.069,19.946),
                 upp = c(2.855,3.698,8.656,13.808,14.987,18.587,20.518),
                 low = c(2.66,3.457,8.276,13.443,14.605,17.532,19.081),
                 species = c("Cricket1","Cricket2", "Katydid1", "Katydid2", "Katydid3", "Katydid4", "Katydid5"))

data$species <- factor(data$species,
                       levels=c("Cricket1","Cricket2", "Katydid1", "Katydid2", "Katydid3", "Katydid4", "Katydid5"))

data %>% ggplot()+
  geom_errorbar(aes(x=Hours, ymin=upp, ymax=low, color = species), width=0.2, size=0.5)+
  geom_point(aes(x = Hours, y = Dom, color = species))+
  geom_smooth(method= "glm", aes(x = Hours, y = Dom), color = "darkgray")+
  geom_text(aes(x = Hours, y = Dom, label = species), size=3.5, 
            hjust=ifelse(data$species=="Katydid4",0.4,0.6),
            vjust=ifelse(data$species=="Katydid4",1.7,-0.9))+
  scale_color_manual(values = c("#e41a1c","#377eb8","#4daf4a","#984ea3","#ff7f00",
                                "#c9b030","#a65628"))+
  labs( x ="Acoustic activity span (h)",
        y = "Average dominant frequency (kHz)"
  )+theme_bw()+
  theme(legend.position = "none")+
  scale_x_continuous(breaks = c(2,4,6,8,10,12))
