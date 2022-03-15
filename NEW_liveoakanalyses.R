library(tidyverse)
library(car)
library(emmeans)
library(ggridges)
library(lubridate)
library(gridExtra)
library(scales)
library(ggpubr)

#### Live Oak Stink Bug Collection Analyses

count19_20<-read_csv("Clean data/speciescountdate.csv")
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
nymp_19_20<-count19_20 %>% 
  filter(Species== "Nymphs" & Crop!="S") %>% 
  group_by(Date, Year, Crop) %>% 
  summarise(mean_count=mean(Count))
nymp_19_20
nymp_19_20$Crop<-dplyr::recode(nymp_19_20$Crop, 
                                      'T'='Tomato-Sorghum',
                                      'TandS'='Tomato+Sorghum')
ggtexttable(nymp_19_20, rows=NULL, theme=ttheme("classic"),
            cols=c("Date" ,"Year","Crop","Mean Nymph Count"))
#ggsave(filename = file.path("Outputs","nymph_movement_table.png"))
write.csv(nymp_19_20, "nymph_data_live_oak_means.csv")

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

##### LINEgraphs for both #####

graph_nymph_adult<- count19_20 %>% 
  filter(Species=="Adults total" | Species== "Nymphs") %>% 
  group_by(Date, Week, Year, Crop, Species) %>% 
  summarise(mean_count=mean(Count)) 
graph_nymph_adult$Crop<-dplyr::recode(graph_nymph_adult$Crop, 
                               'T'='Tomato-Sorghum',
                               'TandS'='Tomato+Sorghum',
                               'S'='Sorghum')
graph_nymph_adult$Date<-as.factor(graph_nymph_adult$Date)
  



ggplot(graph_nymph_adult %>% filter(Year==2019 & Crop!="Sorghum"), aes(x=Date, y=mean_count, color=Species))+
  geom_point()+
  geom_line(aes(group=Species), size=1)+
  facet_wrap(~Crop)+
  scale_color_manual(name="", labels=c("Adults","Nymphs"), 
                    values=c("brown1","darkgoldenrod2"))+
  scale_x_discrete(labels=c("5 June","14 June","20 June","26 June","3 July","10 July","17 July"))+
  labs(title="Nymph and Adult Movement in Tomato Plots 2019")+
  ylab("Mean Stink Bug Count")+
  xlab("")+
  theme(legend.position = "bottom",
        panel.border = element_rect(colour = "gray", fill=NA, size=2),
        text=element_text(size=15),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 35, hjust=1))
ggsave(filename = file.path("Outputs","nymph_adultmovement2019.png"))



ggplot(graph_nymph_adult %>% filter(Year==2020 & Crop!="Sorghum"), aes(x=Date, y=mean_count, color=Species))+
  geom_point()+
  geom_line(aes(group=Species), size=1)+
  facet_wrap(~Crop)+
  labs(title="Nymph and Adult Movement in Tomato Plots 2020")+
  scale_x_date(breaks = as.Date(c("2020-06-03","2020-06-10","2020-06-18","2020-06-24","2020-07-01","2020-07-08", "2020-07-15")), 
               date_labels="%d %b",
               expand = c(0, 5))+
  scale_color_manual(name="", labels=c("Adults","Nymphs"), 
                     values=c("brown1","darkgoldenrod2"))+
  ylab("Mean Stink Bug Count")+
  xlab("")+
  theme(legend.position = "bottom",
        panel.border = element_rect(colour = "gray", fill=NA, size=2),
        text=element_text(size=15),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust=1))
ggsave(filename = file.path("Outputs","nymph_adultmovement2020.png"))


#BAR GRAPHS FOR NYMPHS 

ggplot(graph_nymph_adult %>% filter(Crop!="Sorghum" & Species=="Nymphs" & Year==2019), aes(x=Date, y=mean_count))+
  geom_col(position = "dodge")+
  scale_x_date(breaks=as.Date(c("2019-06-05","2019-06-14","2019-06-20","2019-06-26","2019-07-03","2019-07-10","2019-07-17")) ,
               date_labels="%d %b",
               expand=c(0,5))+
  facet_wrap(~Crop)


###################### REPRODUCTION DATA  ##################################
repro<-read_csv("Clean data/reproduction_data_1920.csv")
repro<- pivot_longer(repro, 
                     unmated_0eggs:mated_15plus,
                     names_to = "repro_status",
                     values_to = "count")
repro$week<-as.factor(repro$week)
#2019

repro_19<-repro %>% 
  filter(year=="2019")

repro_19_means<-repro %>% 
  filter(year=="2019") %>% 
  group_by(week, crop, repro_status) %>% 
  summarise(mean=mean(count))

ggplot(repro_19_means, aes(x=week, y=mean, color=crop, group=crop))+
  geom_point(position = "jitter")+
  geom_smooth()+
  ylim(0,20)+
  facet_grid(crop~repro_status)

ggplot(repro_19, aes(x=week, y=count, color=repro_status, group=repro_status))+
  geom_point(position = "jitter")+
  geom_smooth()+
  ylim(0,20)+
  facet_grid(~crop)

#### difference in mated females s, ts, and t
mated19<-lm(mean~crop, data=repro_19_means %>% filter(repro_status=="mated_15plus"))
mated19
Anova(mated19)
emmeans_mated19<-emmeans(mated19,~crop)
plot(emmeans_mated19)
### no sig diff for 2019

# 2020
repro_20_means<-repro %>% 
  filter(year=="2020") %>% 
  group_by(week, crop, repro_status) %>% 
  summarise(mean=mean(count))

mated20<-lm(mean~crop, data=repro_20_means %>% filter(repro_status=="mated_15plus" & crop!="tomato"))
mated20
Anova(mated20)
emmeans_mated20<-emmeans(mated20,~crop)
plot(emmeans_mated20)


#### probing data  #####
probe<-read_csv("Clean data/feedinginjurydata.csv")
probe$Probes<-as.numeric(probe$Probes)
probe$Weight<-as.numeric(probe$Weight)
probe$Year<-as.factor(probe$Year)
#2019
probe_count <-probe %>% 
  filter(Year=="2019") %>% 
  group_by(Crop) %>% 
  summarise(count=n())

lm_probes19<-lm(Probes~Crop, data=probe %>% filter(Year=="2019"))
Anova(lm_probes19)
em_probes19<-emmeans(lm_probes19,~Crop)
em_probes19
plot(em_probes19)
em_probes19<-as.data.frame(em_probes19)

ggplot(probe %>% filter(Year=="2019"), aes(x=Crop, y=Probes))+
  geom_boxplot()+
  geom_point(position="jitter")
ggplot(probe_19_mean, aes(x=Crop, y=mean_probes))+
  geom_violin()+
  geom_point(position="jitter")
ggplot()+
  geom_pointrange(data=em_probes19, aes(y=emmean, x=Crop, ymin=lower.CL, ymax=upper.CL))

probe_19_mean<-probe %>% 
  filter(Year=="2019") %>% 
  group_by(Crop) %>% 
  summarise(mean_probes=mean(Probes), se_probes=sd(Probes)/sqrt(length(Probes)))
probe_19_mean          
            
lm_meanprobs19<-lm(mean_probes~Crop, data=probe_19_mean)
Anova(lm_meanprobs19)

#inner vs outer
probe_inout_ts<-probe %>% 
  filter(Year=="2019" & Crop=="tomsorg") %>% 
  filter(!is.na(Location)) %>% 
  group_by(Location) %>% 
  summarise(mean_probes=mean(Probes), se_probes=sd(Probes)/sqrt(length(Probes)))
probe_inout_ts
inout_ts<-lm(Probes~ Location, data=probe %>% filter(Year=="2019" & Crop=="tomsorg"))
Anova(inout_ts)

probe_inout_t<-probe %>% 
  filter(Year=="2019" & Crop=="tomato") %>% 
  filter(!is.na(Location)) %>% 
  group_by(Location) %>% 
  summarise(mean_probes=mean(Probes), se_probes=sd(Probes)/sqrt(length(Probes)))
probe_inout_t
inout_t<-lm(Probes~ Location, data=probe %>% filter(Year=="2019" & Crop=="tomato"))
Anova(inout_t)
# 2020

probe_20<-probe %>% 
  filter(Year=="2020" & Crop=="tomato") %>% 
  filter(!is.na(Probes)) %>% 
  group_by(Date) %>% 
  summarise(mean_probes=mean(Probes), se_probes=sd(Probes)/sqrt(length(Probes)))
probe_20

lm_probe_20<-lm(Probes~Crop, data=probe %>% filter(Year=="2020"))
Anova(lm_probe_20)
em_probe_20<-emmeans(lm_probe_20,~Crop)
plot(em_probe_20)


probe_inout_t20<-probe %>% 
  filter(Year=="2020" & Crop=="tomato") %>% 
  filter(!is.na(Location)) %>% 
  group_by(Location) %>% 
  summarise(mean_probes=mean(Probes), se_probes=sd(Probes)/sqrt(length(Probes)))
probe_inout_t20
inout_t20<-lm(Probes~ Location, data=probe %>% filter(Year=="2020" & Crop=="tomato"))
Anova(inout_t20)
em_t20<-emmeans(inout_t20,~Location)
plot(em_t20)

ggplot(probe %>% filter(Year=="2020" & Crop=="tomato"), aes(x=Location, y=Probes))+
  geom_boxplot()+
  geom_point(position = "jitter")


probe_inout_ts20<-probe %>% 
  filter(Year=="2020" & Crop=="tomsorg") %>% 
  filter(!is.na(Probes)) %>% 
  group_by(Location) %>% 
  summarise(mean_probes=mean(Probes), se_probes=sd(Probes)/sqrt(length(Probes)))
probe_inout_ts20
inout_ts20<-lm(Probes~ Location, data=probe %>% filter(Year=="2020" & Crop=="tomsorg"))
Anova(inout_ts20)
em_ts20<-emmeans(inout_ts20,~Location)
plot(em_ts20)

# mean weight of all fruit 
mean_weight<- probe %>% 
  filter(!is.na(Weight)) %>% 
  group_by(Year) %>%
  summarize(mean=mean(Weight))
mean_weight
(55+91.7)/2


##### graph probing for both years
plot_probe<-probe %>% 
  filter(!is.na(Probes)) %>% 
  group_by(Year, Crop) %>% 
  summarise(mean_probes=mean(Probes), 
            se_probes=sd(Probes)/sqrt(length(Probes)))
conf_int_probes<-probe %>% 
  filter(!is.na(Probes)) %>% 
  group_by(Year, Crop) %>% 
  summarise(count_n=n(), 
            mean_probes=mean(Probes), 
            sd_probes=sd(Probes),
            marg_error=qt(.975, df=count_n-1)*sd_probes/sqrt(count_n),
            lower=mean_probes-marg_error,
            upper=mean_probes+marg_error)
conf_int_probes$Crop<-dplyr::recode(conf_int_probes$Crop, 
                               'tomato'='Tomato-Sorghum',
                               'tomsorg'='Tomato+Sorghum')
ggtexttable(conf_int_probes, rows = NULL, theme = ttheme("classic"))
ggsave(filename = file.path("Outputs","conf_int_probes.png"))

ggplot(conf_int_probes, aes(x=Year, y=mean_probes, fill=Crop))+
  geom_col(position=position_dodge(width = .8), width=.5)+
  geom_errorbar(aes(x=Year, ymin=lower, ymax=upper),size=.8, width = .5, position = position_dodge(0.8))+
  scale_fill_manual(name="", 
                    breaks=c('tomsorg','tomato'),
                    labels=c("Tomato+Sorghum","Tomato-Sorghum"),
                    values=c("darkgoldenrod2","brown2"))+
  scale_y_continuous(breaks = seq(0,275, by=50))+
  labs(title="Tomato Fruit Injury")+
  ylab("Mean number of probes")+
  xlab("")+
  theme(legend.position = "bottom",
        panel.border = element_rect(colour = "gray", fill=NA, size=2),
        panel.grid = element_line(color = "gray",size = 0.75), 
        legend.box.background = element_rect(colour = "black"),
        text=element_text(size=15),
        plot.title = element_text(hjust = 0.5))
ggsave(filename = file.path("Outputs","tomatofruitinjury.png"))


