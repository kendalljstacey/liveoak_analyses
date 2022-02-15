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
library(readr)
library(ggtext)
setwd("~/This PC/Documents/R/Live Oak data/live oak data")
nliveoakdata<- read.csv('nliveoakdata.csv')
################################ library ####################################################
nliveoakdata$Date<-factor(nliveoakdata$Date, levels=c("6/10/2020","6/18/2020","6/24/2020","7/1/2020","7/8/2020","7/15/2020"))
nliveoakinout$Date<-factor(nliveoakinout$Date, levels=c("5/27/2020","6/3/2020","6/10/2020","6/18/2020","6/24/2020","7/1/2020","7/8/2020","7/15/2020"))
nliveoakinout$Sex<-factor(nliveoakinout$Sex, levels=c("M","F"))
nliveoaklevels$Date<-factor(nliveoaklevels$Date, levels=c("6/10/2020","6/18/2020","6/24/2020","7/1/2020","7/8/2020","7/15/2020"))
nliveoakmatunmat$Date<-factor(nliveoakmatunmat$Date, levels=c("6/10/2020","6/18/2020","6/24/2020","7/1/2020","7/8/2020","7/15/2020"))
nliveoakfemmale$Date<-factor(nliveoakfemmale$Date, levels=c("6/10/2020","6/18/2020","6/24/2020","7/1/2020","7/8/2020","7/15/2020"))
high<-subset(nliveoakdata, Eggs >= 51)
med<-subset(nliveoakdata, Eggs > 20 & Eggs < 51)
low<-subset(nliveoakdata, Eggs <21)

nliveoakdata$count<- 1
nliveoakdata %>% group_by()
ggplot(nliveoakdata %>% filter(Sex=="F"))+
  geom_boxplot(aes(x=Treatment, y=Eggs, color=Treatment))+
  facet_wrap(~Mated)
#sorghum had some effect on egg count, maybe add jittered points 
############################### species data ###########################################################
ggplot(nliveoakinout, mapping=aes(x="", y=Eggs, fill=Species))+
  geom_bar(stat="identity", width=1)+
  coord_polar("y", start=0)+
  theme_void()+
  scale_fill_brewer()
ggplot(nliveoakinout, mapping=aes(Date, fill=Species))+
  geom_histogram(stat="count", position="fill")+
  theme_ipsum()
##################################### mutate code #########################################################
levels<-nliveoakinout %>% 
  mutate(Level=case_when(Eggs<= 20 ~ "Low", 
                         Eggs>= 21 & Eggs< 50 ~ "Medium",
                         Eggs>= 50 ~ "High"))
summary(levels %>% filter( Mated=="Y" & Species=="NV" & Level== "High") )
summary(levels %>% filter( Mated=="Y" & Species=="NV" & Level== "Medium") )
summary(levels %>% filter( Mated=="Y" & Species=="NV" & Level== "Low") )
summary(levels %>% filter( Mated=="Y" & Species=="NV" & Date== "5/27/2020") )
summary(levels %>% filter( Mated=="Y" & Species=="NV" & Date== "6/3/2020") )
summary(levels %>% filter( Mated=="Y" & Species=="NV" & Date== "6/10/2020") )
summary(levels %>% filter( Mated=="Y" & Species=="NV" & Date== "6/18/2020") )
summary(levels %>% filter( Mated=="Y" & Species=="NV" & Date== "6/24/2020") )
summary(levels %>% filter( Mated=="Y" & Species=="NV" & Date== "7/1/2020") )
summary(levels %>% filter( Mated=="Y" & Species=="NV" & Date== "7/8/2020") )
summary(levels %>% filter( Mated=="Y" & Species=="NV" & Date== "7/15/2020") )

levels$Level<-factor(levels$Level, levels=c("Low","Medium","High"))

sixten<-grobTree(textGrob("2", x=0.1, y=0.95, hjust=0))
sixtwofour<-grobTree(textGrob("2", x=0.1, y=0.95, hjust=0))
sevenone<-grobTree(textGrob("2", x=0.1, y=0.95, hjust=0))
seveneight<-grobTree(textGrob("2", x=0.1, y=0.95, hjust=0))
sevenonefive<-grobTree(textGrob("2", x=0.1, y=0.95, hjust=0))
key<-grobTree((textGrob("n=number of mated females dissected", x=0.1, y=0.95, hjust=0)))
############################# egg count by level with text #####################################################
ggplot()+
  geom_col(levels %>% filter(Mated=="Y"& Species=="NV"), 
           mapping=aes(x=Date, y=Eggs, fill=Level), position="stack")+
  theme(plot.title= element_text(hjust= 0.5))+
  ggtitle("Egg Count by Level")+
  background_grid(major="xy", minor="none")+
  scale_color_manual(values=c("skyblue","orange2","green3"))+
  theme(legend.position = "right", 
        plot.title=element_text(size=20))+
  theme(text = element_text(size=15)) +
  theme(panel.background = element_blank(),                              
        panel.border = element_rect(color="black", fill=NA, size=2)) +           ## change "theme" so the background is blank and the border is thicker
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) +  ## change axis tick marks to make them a little longer
  theme(text = element_text(size=15)) +
  theme(axis.text.x=element_text(angle=40, vjust=1, hjust=1))+
  xlab("Sampling Dates")+
  ylab("Egg Count")
  annotation_custom(sixten, xmin=.4, ymax=135)+
  annotation_custom(sixtwofour, xmin=1.5, ymax=11)+
  annotation_custom(sevenone, xmin=2.6, ymax=57)+
  annotation_custom(seveneight, xmin=3.7, ymax=11)+
  annotation_custom(sevenonefive, xmin=4.8, ymax=185)+
  annotation_custom(key, xmin=.5, ymax=185)

#################################### mated vs. unmated graph #################################################################

matunmat<-gather(nliveoakmatunmat, key=Condition, value=Count,
           c("Unmated Females","Mated Females"))
mytextmatunmat<-grobTree(textGrob("n=33", x=0.1, y=0.95, hjust=0))
ggplot(matunmat, aes(x=factor(Date), y=Count, color=Condition, group=Condition))+
  geom_line(size=1.5)+
  theme(plot.title= element_text(hjust= 0.5))+
  ggtitle("Nezara viridula Females in Tomato Crop")+
  background_grid(major="xy", minor="none")+
  scale_color_manual(values=c("skyblue","orange2"))+
  theme(legend.position = "right", 
        plot.title=element_text(size=20))+
  theme(text = element_text(size=15)) +
  theme(panel.background = element_blank(),                              
        panel.border = element_rect(color="black", fill=NA, size=2)) +           ## change "theme" so the background is blank and the border is thicker
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) +  ## change axis tick marks to make them a little longer
  theme(text = element_text(size=15)) +
  theme(axis.text.x=element_text(angle=40, vjust=1, hjust=1))+
  xlab("Sampling Dates")+
  ylab("Total Number")+
  annotation_custom(mytextmatunmat)+
  theme_ipsum()
  

############################# male vs. female graph #######################################################################
malfem<-gather(nliveoakfemmale, key=Sex, value=Count,
                 c("Males","Females"))
mytextmalfem<-grobTree(textGrob("n=62", x=0.1, y=0.95, hjust=0))
ggplot(malfem, aes(x=Date, y=Count, color=Sex, group=Sex))+
  geom_line(size=1.5)+
  theme(plot.title= element_text(hjust= 0.5))+
  ggtitle("Nezara viridula in Tomato Crop")+
  background_grid(major="xy", minor="none")+
  scale_color_manual(values=c("skyblue","orange2"))+
  theme(legend.position = "right", 
        plot.title=element_text(size=20))+
  theme(text = element_text(size=15)) +
  theme(panel.background = element_blank(),                              
        panel.border = element_rect(color="black", fill=NA, size=2)) +           ## change "theme" so the background is blank and the border is thicker
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) +  ## change axis tick marks to make them a little longer
  theme(text = element_text(size=15)) +
  theme(axis.text.x=element_text(angle=40, vjust=1, hjust=1))+
  xlab("Sampling Dates")+
  ylab("Total Number")+
  annotation_custom(mytextmalfem)+
  theme_ipsum()


############################# high med low eggs not great, not custom ##########################################

ggplot(liveoakdatahighmedlow, aes(x=Date, y=Count, fill=Eggs, group=Eggs))+
  geom_col()+
  theme(plot.title= element_text(hjust= 0.5))+
  ggtitle("Eggs Amount over Time")+
  scale_fill_manual(values=c("skyblue","orange2","green4"))+
  theme(legend.position = "right", 
        plot.title=element_text(size=20))+
  theme(text = element_text(size=15)) +
  theme(panel.background = element_blank(),                              
        panel.border = element_rect(color="black", fill=NA, size=2)) +           ## change "theme" so the background is blank and the border is thicker
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) +  ## change axis tick marks to make them a little longer
  theme(text = element_text(size=15)) +
  theme(axis.text.x=element_text(angle=40, vjust=1, hjust=1))+
  geom_text(aes(label=Count), vjust=0)
########################### clutch size over time ######################################
mytext<-grobTree(textGrob("n=18", x=0.1, y=0.95, hjust=0))
ggplot(nliveoakinout %>% filter( Mated=="Y" & Species=="NV"), aes(x=Date, y=Eggs, color=Eggs, group=Eggs, shape=Row))+
  geom_jitter(size=4)+
  ggtitle("Egg Load of Mated Nezara viridula")+
  theme(plot.title= element_text(hjust= 0.5))+
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
  ylab("Eggs Number")+
  xlab("Sampling Date")+
  annotation_custom(mytext)+
  scale_shape_manual(name="Row", 
                     labels=c("Inner Rows","Outer Rows"), 
                     values=c(17,19))+
  theme_minimal()

###################### diff color diff shape ##############################################################
mytext<-grobTree(textGrob("n=18", x=0.1, y=0.95, hjust=0))
ggplot(nliveoakinout %>% filter( Mated=="Y" & Species=="NV"), aes(x=Date, y=Eggs, color=Row, shape=Row, group=Row))+
  geom_jitter(binaxis='y', stackdir='center', size=4)+
  ggtitle("Egg Load of Mated Nezara viridula")+
  theme(plot.title= element_text(hjust= 0.5))+
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
  ylab("Egg Number")+
  xlab("Sampling Date")+
  annotation_custom(mytext)+
  scale_color_manual(name="Row", 
                     labels=c("Inner Rows","Outer Rows"), 
                     values=c("skyblue","orange2"))+
  scale_shape_manual(name="Row", 
                     labels=c("Inner Rows","Outer Rows"), 
                     values=c(17,19))+
  scale_y_continuous(limits=c(0,100))+
  theme_ipsum()
################# egg load mated NV, not jittered, diff color ##################################################
ggplot(nliveoakinout %>% filter( Mated=="Y" & Species=="NV"), aes(x=Date, y=Eggs, shape=Row, fill=Row))+
  geom_dotplot(binaxis='y', stackdir='center')+
  ggtitle("Egg Load of Mated Nezara viridula")+
  theme(plot.title= element_text(hjust= 0.5))+
  theme(legend.position = "right", 
        plot.title=element_text(size=20))+
  background_grid(major="xy", minor="none")+
  theme(panel.background = element_blank(),                              
        panel.border = element_rect(color="black", fill=NA, size=2)) +           ## change "theme" so the background is blank and the border is thicker
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) +  ## change axis tick marks to make them a little longer
  theme(text = element_text(size=15)) +
  theme(axis.text.x=element_text(angle=40, vjust=1, hjust=1))+
  ylab("Egg Number")+
  xlab("Sampling Date")+
  annotation_custom(mytext)+
  scale_fill_manual(name="Row", 
                     labels=c("Inner Rows","Outer Rows"), 
                     values=c("skyblue","orange2"))+
  scale_shape_manual(name="Row", 
                     labels=c("Inner Rows","Outer Rows"), 
                     values=c(17,19))+
  scale_y_continuous(limits=c(0,100))+
  theme_ipsum()
femNVmate<-nliveoakinout %>% filter( Mated=="Y" & Species=="NV" & Sex=="F")
summary(femNVmate)
head(femNVmate)
############################################# mated vs. egg ###############################################
ggplot(liveoakdata_fema, aes(x=Mated, y=Eggs, fill=Mated))+
  geom_boxplot()+
  ggtitle("Eggs in Unmated vs. Mated")+
  theme(plot.title=element_text(hjust= 0.5))+
    background_grid(major="xy", minor="none")+
  theme(legend.position = "right", 
        legend.background = element_rect(fill="white", size=2, linetype="solid", color="black"))+
  scale_fill_manual(values=c("skyblue","orange2"))+
  theme(legend.position = "none", 
        plot.title=element_text(size=20))+
  theme(text = element_text(size=15)) +
  theme(panel.background = element_blank(),                              
        panel.border = element_rect(color="black", fill=NA, size=2)) +           ## change "theme" so the background is blank and the border is thicker
  theme(axis.ticks.length=unit(0.3, "cm"),  
        axis.text.x = element_text(margin=margin(5,5,5,5,"pt"),colour="black"),
        axis.text.y = element_text(margin=margin(5,5,5,5,"pt"),colour="black")) +  ## change axis tick marks to make them a little longer
  theme(text = element_text(size=15)) +                                            ## make font larger
  theme(legend.position = "right", 
        legend.background = element_rect(fill="white", size=1.25, linetype="solid", color="black")) +
  theme_ipsum()

############################################### Analyses ######################################################
########################################### quantitative analyses ######################################
#row by egg , low sample size 
#boxplot looks good 
ggplot(nliveoakdata %>% filter(Sex=="F" & Mated=="Y"))+
  geom_boxplot(aes(x=Treatment, y=Eggs, fill=Treatment))+
  scale_fill_brewer(palette="Accent")+
  labs(x="Treatment",y="Egg Count")+
  background_grid(major="xy", minor="none")+
  theme(text = element_text(size=15))+
  theme(panel.background = element_blank(),                              
        panel.border = element_rect(color="black", fill=NA, size=1.5))
ggsave("countliveoakgraph.tiff", units="in", width=7, height=6, dpi=300, compression='lzw')


#normality of data 
qqnorm(nliveoakdata$Eggs) # not super normal 
qqline(nliveoakdata$Eggs)


#use this model!!!!!
glmeregg<-glm(Eggs~Treatment+Date, family=quasipoisson, data=nliveoakdata %>% filter(Sex=="F" & Mated=="Y" & Eggs<120))
summary(glmeregg) #correct for overdispersion bc ratio is high, do this by changing family to quasipoisson
Anova(glmeregg) #no treatment effect, 
emglmegg<-emmeans(glmeregg,pairwise~Treatment, type="response") #outlier may be affecting this
summary(emglmegg) #maybe filter out unmated 

##################################### count data analyses ##############################################
countdataliveoak$Date<-factor(countdataliveoak$Date, levels=c("6/10/2020","6/18/2020","6/24/2020","7/1/2020","7/8/2020","7/15/2020"))
ggplot(countdataliveoak)+
  geom_col(aes(x=Date, y=Count, fill=Treatment), position="dodge")+
  scale_fill_brewer(palette="Accent")+
  labs(x="Date",y="Stinkbug Count")+
  background_grid(major="xy", minor="none")+
  theme(text = element_text(size=15))+
  theme(panel.background = element_blank(),                              
        panel.border = element_rect(color="black", fill=NA, size=1.5))
ggsave("countliveoakgraph.tiff", units="in", width=7, height=6, dpi=300, compression='lzw')


qqnorm(countdataliveoak$Count) # not much data but okay i guess 
qqline(countdataliveoak$Count)

#lm for count data 
lmcount<-lm(Count~Date+Treatment, data=countdataliveoak)
summary(lmcount) #7/8, 7/15 have statistical signficance for insect count but treatment does not

emmcount<-emmeans(lmcount,pairwise~Date+Treatment, type="response")
summary(emmcount)


############################################## categorical analyses #######################################
#mated, row
materow<-table(nliveoakdata$Mated,nliveoakdata$Row)
materow #to calculate degrees of freedom, multiply #rows-1 times #columns-1
#RX tables 
#how to know if you should run a Fisher's exact test? only for small sample sizes
#run into more trouble with rx tables because there's more cells and more likely to be too low sample size
#you should only use it if you don't have at least 5 times as many samples as the total number of cells 
#run a fisher test 
fisher.test(materow) # not statistically signficant 
chisq.test(materow,correct=F) #i think sample size too small
mosaicplot(materow, color="orange2")

#mated, date
matedate<-table(nliveoakdata$Mated,nliveoakdata$Date)
matedate
fisher.test(matedate)
mosaicplot(matedate, color="orange2")
#row versus mated barplot 
ggplot(nliveoakdata %>% filter(Sex=="F"), aes(x=Mated, fill=Treatment))+
  geom_histogram(stat="count")+
  theme_ipsum()+
  labs(fill="")
#date versus mated barplot 
ggplot(nliveoakdata %>% filter(Sex=="F"), aes(x=Date, fill=Mated))+
  geom_histogram(stat="count")+
  scale_fill_manual(values=c("skyblue","orange2"))+
  theme_ipsum()
