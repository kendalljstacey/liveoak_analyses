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
library(ggpubr)
getwd()
setwd("C:/Users/kstacey/OneDrive - University of Florida/R Stuff/R/Live Oak data")
liveoakcount<-read.csv("liveoakobjectivestotals.csv")
liveoakcount$condition<-as.factor(liveoakcount$condition)
liveoakcount$week<-as.factor(liveoakcount$week)
###################################### objective 1 #################################################

summary(liveoakcount)
data19<-liveoakcount %>% filter(year=="2019")
sum(data19$count)
data20<-liveoakcount %>% filter(year=="2020")
sum(data20$count)

ggplot(liveoakcount, aes(x=count, fill=crop))+
  geom_histogram(alpha=.7)

ggplot(liveoakcount %>% filter(liveoakcount$count<50), aes(x=week, y=count, color=crop, shape=crop))+
  geom_point(size=2)+
  theme(text=element_text(size=15))+
  scale_color_brewer(palette = "Set1")+
  facet_wrap(~year)

ggplot(liveoakcount %>% filter(liveoakcount$count<50), aes(x=week, y=count, fill=crop))+
  geom_col(position="dodge")+
  theme(text=element_text(size=15))+
  scale_fill_brewer(palette = "Set1")+
  facet_wrap(~year)

png("count_crop_obj1.png")
ggplot(liveoakcount%>% filter(liveoakcount$count<50), aes(x=crop, y=count, fill=crop))+
  geom_boxplot()+
  theme(text=element_text(size=15))+
  scale_fill_brewer(palette = "Set1")+
  facet_wrap(~year)
dev.off()
#### linear model count in tomato versus sorghum
#### 2019
obj1_19<-glm(count~crop+week, data=liveoakcount %>% filter(year=="2019"), family="poisson")
Anova(obj1_19) 
emobj1_19<-emmeans(obj1_19,~crop)
plot(emobj1_19)

#### 2020
obj1_20<-glm(count~crop+week, data=liveoakcount %>% filter(year=="2020"), family="poisson")
Anova(obj1_20)
emobj1_20<-emmeans(obj1_20,~crop)
plot(emobj1_20)


#### linear model tomsorg and sorghum only 
ggplot(liveoakcount %>% filter(condition!="tomato" & count<50), aes(x=week, y=count, fill=condition))+
  geom_col(position = "dodge")+
  facet_wrap(~year)
#### 2019
tomsorg19<-glm(count~crop, data=liveoakcount %>% filter(year=="2019" & condition!="tomato"), family="poisson")
Anova(tomsorg19)

#### 2020
tomsorg20<-glm(count~crop, data=liveoakcount %>% filter(year=="2020" & condition!="tomato"), family="poisson")
Anova(tomsorg20)
##################################### objective 2 #####################################################
#### comparing count of stink bugs in tomato and count in tomato with sorghum 
hist(liveoakcount$count, freq=FALSE)

ggplot(liveoakcount %>% filter(condition== "tomato" | condition=="tomsorg"), aes(x=count, fill=condition))+
  geom_histogram(alpha=.8)

ggplot(liveoakcount %>% filter(liveoakcount$count<50  & condition!= "sorghum"), aes(x=week, y=count, color=condition, shape=condition))+
  geom_point(size=2, alpha=.7)+
  theme(text=element_text(size=15))+
  scale_color_brewer(palette = "Set1")+
  facet_wrap(~year)

ggplot(liveoakcount %>% filter(liveoakcount$count<50 & condition!= "sorghum"), aes(x=week, y=count, fill=condition))+
  geom_col(position="dodge")+
  facet_wrap(~year)

ggplot(liveoakcount%>% filter(liveoakcount$count<50), aes(x=condition, y=count, fill=condition))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Set1")+
  facet_wrap(~year)

#### linear models count in tomsorg, and tomato
#### 2019
obj2_19<-glm(count~condition, data=liveoakcount %>% filter(year=="2019" & condition!= "sorghum"), family="poisson")
Anova(obj2_19) # 2019 no significant difference between count in tom and tomsorg
#plot(obj2_19)
emobj2_19<-emmeans(obj2_19,~condition)
plot(emobj2_19)

#### 2020
obj2_20<-glm(count~condition, data=liveoakcount %>% filter(year=="2020" & condition!= "sorghum"), family="poisson")
Anova(obj2_20) #significant difference between count in tom and tomsorg, more in tomsorg
#plot(obj2_20)
emobj2_20<-emmeans(obj2_20,~condition)
plot(emobj2_20)


##################################### objective 3 ###################################################
mateddata<-read.csv("nliveoakdata.csv")

ggplot(mateddata %>% filter(Mated=="Y"), aes(x=Date, y=Eggs, fill=Treatment))+
  geom_point(size=2)
