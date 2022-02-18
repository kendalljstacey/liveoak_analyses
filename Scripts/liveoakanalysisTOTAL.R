library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(lattice)
library(gridExtra)
library(cowplot)
library(tidyr)
library(ggthemes)
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(car)
library(MuMIn)
library(viridis)
library(hrbrthemes)
library(tibble)
library(caret)
library(ggridges)


############################# count species overall ##################################################

speciescountdate$Date<-factor(speciescountdate$Date, levels=c("5/13/2020","5/20/2020","5/27/2020","6/3/2020","6/10/2020","6/18/2020","6/24/2020","7/1/2020","7/8/2020","7/15/2020"))
speciescountdate$Location<-factor(speciescountdate$Location, levels=c("Outer","Inner","Sorghum"))
speciescountdate$Treatment<-factor(speciescountdate$Treatment, levels=c("T","C","Sorghum"))
adults<-speciescountdate %>% filter(Species=="Adults total"& Location!="Sorghum")
adults


fig2b<-ggplot(speciescountdate %>% filter(Species=="Adults total"& Location!="Sorghum" & Location!="Outer" & Treatment!="Sorghum"))+
  geom_col(aes(x=Date, y=Count, fill=Treatment), size=3, position="dodge")+
  theme_ipsum()+
  ggtitle("Stink Bug Count Inner Rows")+
  theme(legend.position = c(.2,.8),
        legend.background = element_rect(fill="white"))+
  scale_fill_brewer(palette = "Set1")+
  theme(text = element_text(size=15)) +
  background_grid(major="xy", minor="none")+
  theme(panel.background = element_blank(),                              
        panel.border = element_rect(color="black", fill=NA, size=2)) +           ## change "theme" so the background is blank and the border is thicker
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) +  ## change axis tick marks to make them a little longer
  theme(axis.text.x=element_text(angle=40, vjust=1, hjust=1),
        axis.text=element_text(size=14),
        axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16))
fig2b
ggsave("counttreatmentinner.tiff", units="in", width=7, height=6, dpi=300, compression='lzw')

ggplot(speciescountdate %>% filter(Species=="Adults total"& Location!="Sorghum" & Location!="Inner" & Treatment!="Sorghum"))+
  geom_col(aes(x=Date, y=Count, fill=Treatment), size=3, position="dodge")+
  theme_ipsum()+
  ggtitle("Stink Bug Count Outer Rows")+
  theme(legend.position = c(.2,.8),
        legend.background = element_rect(fill="white"))+
  scale_fill_brewer(palette = "Set1")+
  theme(text = element_text(size=15)) +
  background_grid(major="xy", minor="none")+
  theme(panel.background = element_blank(),                              
        panel.border = element_rect(color="black", fill=NA, size=2)) +           ## change "theme" so the background is blank and the border is thicker
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) +  ## change axis tick marks to make them a little longer
  theme(axis.text.x=element_text(angle=40, vjust=1, hjust=1),
        axis.text=element_text(size=14),
        axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16))
ggsave("counttreatmentouter.tiff", units="in", width=7, height=6, dpi=300, compression='lzw')


ggplot(speciescountdate %>% filter(Species=="Adults total"), aes(x=Date, y=Count, color=Location))+
  geom_jitter(size=3)+
  scale_color_brewer(palette = "Accent")+
  theme(legend.position = "right", 
        plot.title=element_text(size=20))+
  background_grid(major="xy", minor="none")+
  theme(text = element_text(size=15)) +
  theme(panel.background = element_blank(),                              
        panel.border = element_rect(color="black", fill=NA, size=2)) +           ## change "theme" so the background is blank and the border is thicker
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) +  ## change axis tick marks to make them a little longer
  theme(text = element_text(size=15)) +
  theme(axis.text.x=element_text(angle=40, vjust=1, hjust=1))+
  facet_wrap(~Treatment)


fig2a<-ggplot(speciescountdate %>% 
              filter(Species=="Adults total" & Treatment=="T" & Count<70), 
              aes(x=Date, y=Count, color=Location))+
  geom_point(size=3)+
  theme_ipsum()+
  ggtitle("Stink Bug Count Treatment Plot Over Time")+
  scale_color_brewer(palette = "Set1")+
  theme(legend.position = "right", 
        plot.title=element_text(size=20),
        legend.background = element_rect(fill="white"))+
  background_grid(major="xy", minor="none")+
  theme(legend.position = c(.80,.8),
        legend.background = element_rect(fill="white"))+
  theme(text = element_text(size=15)) +
  theme(panel.background = element_blank(), 
        panel.border = element_rect(color="black", fill=NA, size=2)) +           ## change "theme" so the background is blank and the border is thicker
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) +  ## change axis tick marks to make them a little longer
  theme(text = element_text(size=15)) +
  theme(axis.text.x=element_text(angle=40, vjust=1, hjust=1),
        axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16))
fig2a
ggsave("countlocationtreatment.tiff", units="in", width=7, height=6, dpi=300, compression='lzw')


grid.arrange(fig2a, fig2b, ncol=2)

qqnorm(speciescountdate$Count)
qqline(speciescountdate$Count)

ggplot(data=speciescountdate %>% filter(Species=="Nymphs" & Location!="Sorghum"))+
  geom_boxplot(aes(x=Treatment, y=Count, fill=Location))+
  scale_fill_brewer(palette = "Set2")+
  theme(legend.position = "right", 
        plot.title=element_text(size=20),
        legend.background = element_rect(fill="white"))+
  background_grid(major="xy", minor="none")+
  theme(legend.position = c(.84,.75),
        legend.background = element_rect(fill="white"))+
  theme(text = element_text(size=15)) +
  theme(panel.background = element_blank(), 
        panel.border = element_rect(color="black", fill=NA, size=2)) +           ## change "theme" so the background is blank and the border is thicker
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) +  ## change axis tick marks to make them a little longer
  theme(text = element_text(size=15)) +
  theme(axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16))

ggplot(data=speciescountdate %>% filter(Species=="Nymphs" & Location!="Sorghum"), 
                                        aes(x=Date, y=Count, color=Location, group=Location))+
  geom_line()+
  geom_point()

lmnymph<-lm(Count~Treatment*Location+Date, data=speciescountdate %>% filter(Species=="Nymphs"& Location!="Sorghum"))
Anova(lmnymph)
emnymph<-emmeans(lmnymph, ~Location:Date:Treatment)
emnymph
plot(emnymph)

lmcount<-lm(Count~Treatment+Location+Date, data=speciescountdate %>% filter(Species=="Adults total"& Location!="Sorghum"))
Anova(lmcount) #Date is a good predictor for count 
plot(lmcount)
emcount<-emmeans(lmcount,~Treatment:Date)
emcount
plot(emcount)
sim_lmcount <- simulateResiduals(fittedModel = lmcount, n = 250)
plot(sim_lmcount) #this shows that this is a good model for this data, no serious problems with data 

y1=speciescountdate %>% filter(Treatment== "T")
y2=speciescountdate %>% filter(Treatment== "C")
t.test(x=y1$Count,y=y2$Count,alternative="two.sided",var.equal=T) #not significant 

y1=speciescountdate %>% filter(Location=="Inner")
y2=speciescountdate %>% filter(Location=="Outer")
t.test(x=y1$Count,y=y2$Count,alternative="two.sided",var.equal=T) # count in inner rows is marginally larger than count in outsdie rows  


############################### model validation ################################
adults<- speciescountdate %>% filter(Species=="Adults total")
trainingsamples<-adults$Count %>% 
  createDataPartition(p=.8, list=FALSE)
train.data<-adults[trainingsamples,]
test.data<-adults[-trainingsamples,]
model<-lm(Count~Treatment+Location+Date, data=train.data)
predictions<-model %>%  predict(test.data)
data.frame(R2 = R2(predictions, test.data$Count),
           RMSE = RMSE(predictions, test.data$Count),
           MAE = MAE(predictions, test.data$Count)) #whichever of these models that gives lowest score is the preferred model 
#Dividing the RMSE by the average value of the outcome variable will give you the prediction error rate, which should be as small as possible
RMSE(predictions, test.data$Count)/mean(test.data$Count) #it's .6311 which is low 
############################### feeding damage ########################################################
str(feedinginjurydata)
feedinginjurydata$Location<-factor(feedinginjurydata$Location, levels=c("Inner", "Outer"))
feedinginjurydata$Date<-factor(feedinginjurydata$Date, levels=c("6/10/2020","6/18/2020","6/24/2020","7/1/2020","7/8/2020","7/15/2020"))
feedinginjurydata$Damage<-factor(feedinginjurydata$Damage, levels=c("U","D","SD","VSD"))
feedinginjurydata$Treatment<-factor(feedinginjurydata$Treatment, levels=c("T","C"))
feedinginjurydata$Color<-factor(feedinginjurydata$Color, levels=c("G", "B", "T","P", "LR","R"))
feedinginjurydata$Probes<-as.numeric(as.character(feedinginjurydata$Probes))
feedinginjurydata$Weight<-as.numeric(as.character(feedinginjurydata$Weight))

################# probe means ###########################################
ggplot(probemeans %>% filter(Date!='NA'), aes(x=Date, y=Probes, color=Row, group=Row))+
  geom_line(size=1)
#########################################################################
ggplot(feedinginjurydata %>% filter(Damage!="NA"), aes(x="", y="count", fill=Damage))+
  geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0)+
  theme_void()+
  scale_fill_brewer(palette = "Set1", 
                    labels=c("Undamaged", "Damaged", "Seriously Damaged", "Very Seriously Damaged"))+
  theme(text = element_text(size=15)) +
  theme(axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16))+
  theme(panel.background = element_blank()) +           ## change "theme" so the background is blank and the border is thicker
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) +  ## change axis tick marks to make them a little longer
  theme(text = element_text(size=15)) +
  theme(axis.text.x=element_text(angle=40, vjust=1, hjust=1))
ggsave("damagepie.tiff", units="in", width=7, height=6, dpi=300, compression='lzw')


fig3a<-ggplot(feedinginjurydata, aes(x=Date, y=Probes, fill=Date))+
  geom_violin(alpha=.9)+
  geom_jitter(size=.4)+
  theme_ipsum()+
  ggtitle("Probing Over Time")+
  scale_fill_brewer(palette = "Set1")+
  theme(legend.position = "none",
        plot.title=element_text(size=20))+
  background_grid(major="xy", minor="none")+
  theme(text = element_text(size=15)) +
  theme(axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16))+
  theme(panel.background = element_blank(),                              
        panel.border = element_rect(color="black", fill=NA, size=2)) +           ## change "theme" so the background is blank and the border is thicker
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) +  ## change axis tick marks to make them a little longer
  theme(text = element_text(size=15)) +
  theme(axis.text.x=element_text(angle=40, vjust=1, hjust=1))
fig3a
ggsave("probeviolin.tiff", units="in", width=7, height=6, dpi=300, compression='lzw')

ggplot(feedinginjurydata)+
  geom_boxplot(aes(x=Treatment, y=Probes, fill=Location), alpha=.8)+
  scale_fill_brewer(palette = "Set2")+
  theme(plot.title=element_text(size=20))+
  background_grid(major="xy", minor="none")+
  theme(text = element_text(size=15)) +
  theme(axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16))+
  theme(panel.background = element_blank(),                              
        panel.border = element_rect(color="black", fill=NA, size=2)) +           ## change "theme" so the background is blank and the border is thicker
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) +  ## change axis tick marks to make them a little longer
  theme(text = element_text(size=15))

ggplot(feedinginjurydata)+
  geom_histogram(aes(x=Probes, fill=Treatment), alpha=.8, position=position_dodge())+
  scale_fill_brewer(palette = "Set2")+
  theme(plot.title=element_text(size=20))+
  background_grid(major="xy", minor="none")+
  theme(text = element_text(size=15)) +
  theme(axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16))+
  theme(panel.background = element_blank(),                              
        panel.border = element_rect(color="black", fill=NA, size=2)) +           ## change "theme" so the background is blank and the border is thicker
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) +  ## change axis tick marks to make them a little longer
  theme(text = element_text(size=15))+
  facet_wrap(~Location)

ggplot(feedinginjurydata)+
  geom_density(aes(x=Probes, fill=Treatment), alpha=.6, position='identity')+
  scale_fill_brewer(palette = "Set2")+
  theme(plot.title=element_text(size=20))+
  background_grid(major="xy", minor="none")+
  theme(text = element_text(size=15)) +
  theme(axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16))+
  theme(panel.background = element_blank(),                              
        panel.border = element_rect(color="black", fill=NA, size=2)) +           ## change "theme" so the background is blank and the border is thicker
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) +  ## change axis tick marks to make them a little longer
  theme(text = element_text(size=15))+
  facet_wrap(~Location)

fig3b<-ggplot(feedinginjurydata)+
  geom_boxplot(aes(x=Date, y=Probes, fill=Location))+
  facet_wrap(~Treatment)+
  scale_fill_brewer(palette = "Set1")+
  theme(plot.title=element_text(size=20))+
  theme_ipsum()+
  background_grid(major="xy", minor="none")+
  theme(text = element_text(size=15)) +
  theme(axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16))+
  theme(panel.background = element_blank(),                              
        panel.border = element_rect(color="black", fill=NA, size=2),
        legend.position = c(.1,.85)) +           ## change "theme" so the background is blank and the border is thicker
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) +  ## change axis tick marks to make them a little longer
  theme(text = element_text(size=15))+
  theme(axis.text.x=element_text(angle=40, vjust=1, hjust=1))
fig3b
ggsave("probeboxplot.tiff", units="in", width=7, height=6, dpi=300, compression='lzw')

ggplot(feedinginjurydata %>% filter(Location!= "Outer"))+
  geom_boxplot(aes(x=Date, y=Probes, fill=Treatment))+
  scale_fill_brewer(palette = "Set1")+
  theme(plot.title=element_text(size=20))+
  theme_ipsum()+
  ggtitle("Probing in Inner Rows")+
  background_grid(major="xy", minor="none")+
  theme(text = element_text(size=15)) +
  theme(axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16))+
  theme(panel.background = element_blank(),                              
        panel.border = element_rect(color="black", fill=NA, size=2),
        legend.position = c(.1,.85)) +           ## change "theme" so the background is blank and the border is thicker
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) +  ## change axis tick marks to make them a little longer
  theme(text = element_text(size=15))+
  theme(axis.text.x=element_text(angle=40, vjust=1, hjust=1))
ggsave("inneronlyboxplot.tiff", units="in", width=7, height=6, dpi=300, compression='lzw')

qqnorm(feedinginjurydata$Probes)
qqline(feedinginjurydata$Probes)

control<-feedinginjurydata %>% filter(Treatment=="C")
trt<-feedinginjurydata %>% filter(Treatment=="T")

mean(control$Probes)
mean(trt$Probes)

lmerfeed<-lm(Probes~Date+Treatment*Location, data=feedinginjurydata)
Anova(lmerfeed) #color , location, and date had some effect on probes but the interaction between location and trt was also signfiicant 
emfeed<-emmeans(lmerfeed,~Location:Treatment)
emfeed
plot(emfeed)


lmcolor<-lm(Probes~ Color, data=feedinginjurydata)
Anova(lmcolor)
emcolor<-emmeans(lmcolor,~Color)
emcolor
plot(emcolor)

aov<-aov(Probes~ Color, data=feedinginjurydata)
summary(aov)

y1=feedinginjurydata %>% filter(Treatment=="T")
y2=feedinginjurydata %>% filter(Treatment== "C")
t.test(x=y1$Probes,y=y2$Probes,alternative="two.sided",var.equal=T) # there's a signficant difference between probes in contorl and trt, trt was signficantly higher 

y1=feedinginjurydata %>% filter(Color== "G" |Color==  "B" |Color== "T")
y2=feedinginjurydata %>% filter(Color== "P" |Color==  "LR" |Color== "R")
t.test(x=y1$Probes,y=y2$Probes,alternative="two.sided",var.equal=T) # there's a signficant difference between probes in contorl and trt, trt was signficantly higher 

ggplot(feedinginjurydata %>% filter(Color!="NA"))+
  geom_violin(aes(x=Color, y=Probes, fill=Color))+
  geom_jitter(aes(x=Color, y=Probes))+
  theme_ipsum()+
  scale_fill_brewer(palette = "Set2", 
                    labels=c("Green", "Breaking", "Turning", "Pink", "Light Red", "Red"))+
  theme(plot.title=element_text(size=20))+
  background_grid(major="xy", minor="none")+
  theme(text = element_text(size=15)) +
  theme(axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16))+
  theme(panel.background = element_blank(),                              
        panel.border = element_rect(color="black", fill=NA, size=2),
        legend.position = "right") +           ## change "theme" so the background is blank and the border is thicker
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) +  ## change axis tick marks to make them a little longer
  theme(text = element_text(size=15))

ggplot(feedinginjurydata %>% filter(Color!="NA"))+
  geom_col(aes(x=Color, y=Probes, fill=Treatment), position="dodge")+
  scale_fill_brewer(palette="Set2")+
  scale_x_discrete(labels=c("G"="Green", "B"="Breaking", "T"="Turning", "P"="Pink", "LR"="Light Red", "R"="Red"))+
  theme(plot.title=element_text(size=20))+
  background_grid(major="xy", minor="none")+
  theme(text = element_text(size=15)) +
  theme(axis.title.x=element_text(size=16),
        axis.title.y=element_text(size=16))+
  theme(panel.background = element_blank(),                              
        panel.border = element_rect(color="black", fill=NA, size=2)) +           ## change "theme" so the background is blank and the border is thicker
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) +  ## change axis tick marks to make them a little longer
  theme(text = element_text(size=15))

