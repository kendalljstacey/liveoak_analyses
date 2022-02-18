library(tidyverse)
library(car)
library(emmeans)
library(ggridges)
library(lubridate)


#### Live Oak Stink Bug Collection Analyses

count19_20<-read_csv("speciescountdate.csv")
count19_20$Count<-as.numeric(count19_20$Count)
count19_20$Date<-mdy(count19_20$Date)
count19_20$Year<-as.factor(count19_20$Year)

count20<- count19_20 %>% 
  filter(Year=="2020")

count19<- count19_20 %>% 
  filter(Year=="2019")

#### analyzing years together ####
ggplot(count19_20 %>% filter(Crop!="S"), aes(x=Year, y=Count, fill=Crop))+
  geom_col(position = "dodge")

#### stink bug count in S and TS ####
# 2019
count19_means<-count19 %>% 
  filter(Species=="Adults total" | Species=="Nymphs") %>% 
  group_by(Date, Year, Species, Crop) %>% 
  summarise(mean_count= mean(Count))

lm_count_S_TS_19<-lm(mean_count~Crop, data=count19_means %>% filter(Crop!="T" & Species!="Nymphs"))
Anova(lm_count_S_TS_19) #slightly significant

em_count_S_TS_19<-emmeans(lm_count_S_TS_19,~Crop)
plot(em_count_S_TS_19)

ggplot(count19_means %>% filter(Crop!="T"), aes(x=Crop, y=mean_count))+
  geom_boxplot()

graph_19<-count19 %>% 
  filter(Species=="Adults total") %>% 
  group_by(Date, Crop, Species)%>% 
  summarise(mean_count= mean(Count))
  

ggplot(data=graph_19, aes(x=Date, y=mean_count, color=Crop, group=Crop))+
  geom_point(size=2)+
  geom_line(size=1)

# 2020
count20_means<-count20 %>% 
  filter(Species=="Adults total" | Species=="Nymphs") %>% 
  group_by(Date, Year, Species, Crop) %>% 
  summarise(mean_count= mean(Count))

lm_count_S_TS_20<-lm(mean_count~Crop, data=count20_means %>% filter(Crop!="T" & Species!="Nymphs"))
Anova(lm_count_S_TS_20)
#no significant difference

em_count_S_TS_20<-emmeans(lm_count_S_TS_20,~Crop)
plot(em_count_S_TS_20)

ggplot(count20_means, aes(x=Crop, y=Count))+
  geom_point(position = "jitter")



#### stink bug count in T and TS  #####
# 2019

lm_count_T_TS_19<-lm(mean_count~Crop, data=count19_means %>% filter(Crop!="S" & Species!="Nymphs"))
Anova(lm_count_T_TS_19) #not significant

em_count_T_TS_19<-emmeans(lm_count_T_TS_19,~Crop)
plot(em_count_T_TS_19)


# 2020

lm_count_T_TS_20<-lm(mean_count~Crop, data=count20_means %>% filter(Crop!="S" & Species!="Nymphs"))
Anova(lm_count_T_TS_20)
#no significant difference

em_count_T_TS_20<-emmeans(lm_count_T_TS_20,~Crop)
plot(em_count_T_TS_20)


#### nymph movement ####
#2019
ad_nymp_19<-count19 %>% 
  filter(Species=="Adults total" | Species== "Nymphs") %>% 
  group_by(Date, Species) %>% 
  summarise(mean_count=mean(Count))

ggplot(ad_nymp_19, aes(x=Date, y=mean_count, color=Species))+
  geom_point(size=2)+
  geom_line(size=1)+
  theme(axis.text.x = element_text (angle = 30, vjust = 1, hjust=1))+
  labs(title="2019 Adult and Nymph Count")+
  ylab("Mean Count")+
  xlab("")+
  scale_color_brewer(palette="Set2")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
        panel.grid = element_line(color = "gray",
                                  size = 0.75),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size=12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))
ggsave(filename = file.path("Outputs","nymph_adult_2019.png"))

ad_nymp_crop_19<-count19 %>% 
  filter(Species=="Adults total" | Species== "Nymphs") %>% 
  group_by(Date, Species, Crop) %>% 
  summarise(mean_count=mean(Count))

ggplot(ad_nymp_crop_19 %>% filter(Crop!="T"), aes(x=Date, y=mean_count, color=Crop))+
  geom_point(size=2)+
  geom_line(size=1)+
  theme(axis.text.x = element_text (angle = 30, vjust = 1, hjust=1))+
  labs(title="2019 Adult and Nymph Count")+
  ylab("Mean Count")+
  xlab("")+
  scale_color_brewer(palette="Set2")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
        panel.grid = element_line(color = "gray",
                                  size = 0.75),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size=12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))+
  facet_wrap(~Species)
ggsave(filename = file.path("Outputs","nymph_adult_crop_2020.png"))

#2020
ad_nymp_20<-count20 %>% 
  filter(Species=="Adults total" | Species== "Nymphs") %>% 
  group_by(Date, Species) %>% 
  summarise(mean_count=mean(Count))

ggplot(ad_nymp_20, aes(x=Date, y=mean_count, color=Species))+
  geom_point(size=2)+
  geom_line(size=1)+
  theme(axis.text.x = element_text (angle = 30, vjust = 1, hjust=1))+
  labs(title="2020 Adult and Nymph Count")+
  ylab("Mean Count")+
  xlab("")+
  scale_color_brewer(palette="Set2")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
        panel.grid = element_line(color = "gray",
                                  size = 0.75),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size=12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))
ggsave(filename = file.path("Outputs","nymph_adult_2020.png"))

ad_nymp_crop_20<-count20 %>% 
  filter(Species=="Adults total" | Species== "Nymphs") %>% 
  group_by(Date, Species, Crop) %>% 
  summarise(mean_count=mean(Count))

ggplot(ad_nymp_crop_20 %>% filter(Crop!="T"), aes(x=Date, y=mean_count, color=Crop))+
  geom_point(size=2)+
  geom_line(size=1)+
  theme(axis.text.x = element_text (angle = 30, vjust = 1, hjust=1))+
  labs(title="2020 Adult and Nymph Count")+
  ylab("Mean Count")+
  xlab("")+
  scale_color_brewer(palette="Set2")+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=2),
        panel.grid = element_line(color = "gray",
                                  size = 0.75),
        axis.text.x = element_text(size = 12),
        axis.title.y = element_text(size=12),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))+
  facet_wrap(~Species)
ggsave(filename = file.path("Outputs","nymph_adult_crop_2020.png"))

