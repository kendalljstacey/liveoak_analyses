library(tidyverse)
library(car)
library(emmeans)


#### stink bug count in S and TS 2020 ####

count20<-read_csv("speciescountdate.csv")

str(count20)

count20<-count20 %>% 
  filter(Species!="Adults total" & Species!="Nymphs")

lm_count_S_TS_20<-lm(Count~Crop, data=count20 %>% filter(Crop!="T"))
Anova(lm_count_S_TS_20)
#no significant difference

em_count_S_TS_20<-emmeans(lm_count_S_TS_20,~Crop)
plot(em_count_S_TS_20)

ggplot(count20, aes(x=Crop, y=Count))+
  geom_point(position = "jitter")

#### stink bug count in T and TS 2020 #####

lm_count_T_TS_20<-lm(Count~Crop, data = count20 %>% filter(Crop!="S"))
Anova(lm_count_T_TS_20)
# no sig difference 