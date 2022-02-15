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
setwd("C:/Users/kstacey/OneDrive - University of Florida/R Stuff/R")
count20192020<-read.csv("C:/Users/kstacey/OneDrive - University of Florida/R Stuff/R/Live Oak data/count20192020.csv")
longercount<-count20192020 %>% 
  pivot_longer(cols=S:T, names_to = "condition", values_to = "count")
longercount$Week<-factor(longercount$Week, levels=c("1","2","3","4","5","6","7"))
longercount$Year<-factor(longercount$Year, levels=c("2019","2020"))
############################################## normality ###############################################

ggdensity(count20192020)
ggqqplot(count20192020)
qqnorm(longercount$count)
qqline(longercount$count)
histogram(longercount$count)
########################################### both years data #############################################################


count2019<-count20192020 %>% filter(Year=="2019")
t.test(x=count2019$TandS, y=count2019$T,alternative="two.sided",var.equal=T) #not significant 

count2020<-count20192020 %>% filter(Year=="2020")
t.test(x=count2020$TandS, y=count2020$S,alternative="two.sided",var.equal=T) #not significant 

################################## linear models ########################################################
count19<-glm(count~condition, data=longercount %>% filter(Year==2019 & condition!="S"), family=poisson)
Anova(count19)
summary(count19)
em19<-emmeans(count19,~condition)
em19
plot(em19)

count20<-glm(count~condition, data=longercount %>% filter(Year==2020 & condition!="T"), family=poisson)
Anova(count20)
summary(count20)
em20<-emmeans(count20,~condition)
em20
plot(em20)

ggplot(longercount, aes(x=condition, y=count, fill=Year))+
  geom_col(position="dodge")
ggplot(longercount, aes(x=Week, y=count, color=condition))+
  geom_point(position="dodge", size=3, alpha=.5)+
  facet_wrap(~Year)

