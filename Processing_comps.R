library(tidyverse)

#full data set for P0-P10
All <- read.csv("") #set path to location of file Data_quant.csv

#Log transformation for all response variables 
All$logTime <- log(All$Time)
All$logTC <- log(All$pct_TC)
All$logOC <- log(All$pct_SOC)
All$logIC <- log(All$pct_SIC)

#subset for sieving (P0, P1, P2, P3) analyses
Goldv <- All[c(1:240), ]

#defining factors
Goldv$Site <- as.factor(Goldv$Site)
Goldv$Procedure <- as.factor(Goldv$Procedure)
Goldv$RepID <- as.factor(Goldv$RepID)
str(Goldv)

#Statistical analyses for plant and rock material removed by sieving procedure 

library(lme4)
library(lmerTest)
library(pbkrtest)
library(emmeans)

#square root transformation
Goldv$sqrtplant <- sqrt(Goldv$pct_plant)
#mixed linear  for % plant material removed
Modelpp <- lmer(sqrtplant~Site*Procedure + (1|RepID), data = Goldv)
#checking residuals and normality
plot(Modelpp)
qqnorm(resid(Modelpp))
qqline(resid(Modelpp), col="black")
anova(Modelpp, ddf="Kenward-Roger")
#pairwise comparisons
emmeans(Modelpp, pairwise ~ Procedure)
emmeans(Modelpp, pairwise ~ Procedure|Site)

#square root transformation
Goldv$sqrtrock <- sqrt(Goldv$pct_rock)
#mixed linear model for % rock removed
Modelpr <- lmer(sqrtrock~Site*Procedure + (1|RepID), data = Goldv)
#checking residuals and normality 
plot(Modelpr)
qqnorm(resid(Modelpr))
qqline(resid(Modelpr), col="black")
anova(Modelpr, ddf="Kenward-Roger")
#pairwise comparisons
emmeans(Modelpr, pairwise ~ Procedure)
emmeans(Modelpr, pairwise ~ Procedure|Site)

#Creating the stacked bar graph figure for % total coarse material removed

cstacked2 <- read.csv("") # set path to location of file coarsesum2.csv
#arranging sites by lowest to highest % total coarse material removed from soil
cstacked2$Site <- factor(cstacked2$Site, levels = c('J','I', 'E', 'D', 'B', 'L','G','A','K','F','C','H'))
#creating separate figures to adjust y-axis scales
cslow <- subset(cstacked2,  class == "low")
cshigh <- subset(cstacked2,  class == "high")

library(ggplot2)

#figure for low % total coarse material
legend_title <- "Coarse fraction"
coarseplotL <- ggplot(data = cslow, aes(x=Procedure, y = mean, fill=Coarse_fraction)) +
  geom_bar(stat = "identity", position="stack") +
  scale_fill_manual("Coarse material", values=c("rock" = "bisque4", "plant" = "darkgreen")) +
  geom_errorbar(aes(x=Procedure, ymin=y_pos-se , ymax= y_pos+se), width=.4, position=position_dodge(.9))+
  ylim(0,1.5)+
  facet_wrap(~Site, nrow=1) +
  ylab(bquote('% coarse material removed (soil mass)')) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=14), 
        strip.text.x = element_text(size = 12,face="bold"),
        axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
coarseplotL

#figure for high % total coarse material
coarseplotH <- ggplot(data = cshigh, aes(x=Procedure, y = mean, fill=Coarse_fraction)) +
  geom_bar(stat = "identity", position="stack") +
  scale_fill_manual("Coarse material", values=c("rock" = "bisque4", "plant" = "darkgreen")) +
  geom_errorbar(aes(x=Procedure, ymin=y_pos-se , ymax= y_pos+se), width=.4, position=position_dodge(.9))+
  ylim(0,17)+ #defining y-axis scale
  facet_wrap(~Site, nrow=1) +
  ylab(bquote('% coarse material removed (soil mass)')) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=14), 
        strip.text.x = element_text(size = 12,face="bold"),
        axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
coarseplotH

#creating one figure
library(patchwork)
coarse <- coarseplotL + coarseplotH+
  plot_layout(nrow=2, guides="collect")
coarse

#labeling panels
fcoarse <- coarse+plot_annotation(tag_levels = list(c("(a)", "(b)")))
fcoarse
ggsave("fcoarse.png", dpi= 300, width = 7, height = 9, fcoarse)

#Regression figure for std dev vs % difference in plant material removed from highest %
CoarseSD <- read.csv("") #set path to location of file stddev_reg.csv

F = as.formula(y~x)

library(ggpmisc)
library(ggpubr)

#to remove 0 values below (CoarseSD[which(CoarseSD$d_plant>0),]
diffc <- ggplot(CoarseSD[which(CoarseSD$d_plant>0),], aes(x=d_plant, y = stddev_OC, color=Procedure)) +
  geom_point()+
  scale_color_manual("Procedure", values=c("P0" = "#332288", "P1"="#117733", "P2" = "#44aa99","P3"= "#88ccee"))+
  scale_fill_manual("Procedure", values=c("P0" = "#332288", "P1"="#117733", "P2" = "#44aa99","P3"= "#88ccee"))+
  geom_smooth(data=subset(CoarseSD, Procedure == "P3"),method = "lm", se=FALSE, formula = F) +
  stat_regline_equation(data=subset(CoarseSD, Procedure == "P3"),
                        label.x = c(-.15), label.y = c(1.0),
                        aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
                        formula=F,size=5, color="black")+
  xlab(bquote('Difference from highest % plant removed'))+
  ylab(bquote('STD DEV % SOC')) +
  annotate("text", x= -.05, y= .9, label= "p = 0.026", col="black", size = 5)+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=14), 
        strip.text.x = element_text(size = 14,face="bold"),
        axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
diffc

ggsave("stdOCvp.png", dpi= 300, width = 9, height = 7, diffc)

#regression equation and test for significance
dpOC = lm(stddev_OC ~ d_plant, data=CoarseSD[CoarseSD$Procedure=="P3",])
summary(dpOC)

#Anova and pairwise comparisons effect of sieving on % total C, inorganic C, and organic C

#Total C using mixed model
ModelTC <- lmer(logTC~Site*Procedure + (1|RepID), data = Goldv)
plot(ModelTC)
qqnorm(resid(ModelTC))
qqline(resid(ModelTC), col="black")
anova(ModelTC, ddf="Kenward-Roger")
emmeans(ModelTC, pairwise ~ Procedure)
emmeans(ModelTC, pairwise ~ Procedure|Site)

#Inorganic C using mixed model
#subset for soils with inorganic C
GoldvICfull <- subset(Goldv, Fizz == "P")
ModelIC <- lmer(logIC~Site*Procedure + (1|RepID), data = GoldvICfull)
plot(ModelIC)
qqnorm(resid(ModelIC))
qqline(resid(ModelIC), col="black")
summary(ModelIC)
anova(ModelIC, ddf="Kenward-Roger")
emmeans(ModelIC, pairwise ~ Procedure)

#Organic C using mixed model
ModelOC <- lmer(logOC~Site*Procedure + (1|RepID), data = Goldv)
plot(ModelOC)
qqnorm(resid(ModelOC))
qqline(resid(ModelOC), col="black")
anova(ModelOC, ddf="Kenward-Roger")
emmeans(ModelOC, pairwise ~ Procedure)


#Boxplot figures for % TC and SOC (swapping out variables) for sieving procedures

#to display with 2 different y-axis scales
Goldv$Site <- factor(Goldv$Site, levels = c('F','E', 'C', 'G', 'I', 'D','K','L','B','J','A','H'))
GoldvlowC <- Goldv[c(11:35, 41:45, 71:95, 101:105, 131:155, 161:165, 191:215, 221:225), ]
GoldvhighC <- Goldv[c(1:10,36:40,46:70,96:100,106:130,156:160,166:190,216:220,226:240), ]

#plot for low total C soils
plotlOC <-ggplot(data=GoldvlowC, aes(x=Site, y=pct_SOC, colour=Procedure)) + 
  geom_boxplot()+ 
  scale_color_manual("Procedure", values=c("P0" = "#332288", "P1"="#117733", "P2" = "#44aa99","P3"= "#88ccee"))+
  ylim(.7,1.8)+
  ylab(bquote('% SOC'))+
  xlab(bquote('Soil'))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=14), 
        strip.text.x = element_text(size = 14,face="bold"))

plotlOC

library(scales)
plothOC <-ggplot(data=GoldvhighC, aes(x=Site, y=pct_SOC, colour=Procedure)) + 
  geom_boxplot() + 
  scale_color_manual("Procedure", values=c("P0" = "#332288", "P1"="#117733", "P2" = "#44aa99","P3"= "#88ccee"))+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  ylab(bquote('% SOC'))+ 
  xlab(bquote('Soil'))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=14), 
        strip.text.x = element_text(size = 14,face="bold"))
plothOC

#creating one figure
OC <- plotlOC + plothOC +
  plot_layout(ncol=2, guides="collect")
OC 
fOC <- OC+plot_annotation(tag_levels = list(c("(a)", "(b)")))
fOC
ggsave("OC_sieve.png", dpi= 300, width = 9, height = 6, fOC )

#boxplot figure for % inorganic C for sieving procedures
plotlIC <-ggplot(data=GoldvICfull, aes(x=Site, y=pct_SIC, colour=Procedure)) + 
  geom_boxplot()+ 
  scale_color_manual("Procedure", values=c("P0" = "#332288", "P1"="#117733", "P2" = "#44aa99","P3"= "#88ccee"))+
  ylab(bquote('% SIC'))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=14), 
        strip.text.x = element_text(size = 14,face="bold"))

plotlIC
ggsave("IC_sieve.png", dpi= 300, width = 9, height = 6,plotlIC )

#Anova and pairwise comparisons effect of fine grinding on % total C, inorganic C, and organic C

#subset for fine grinding treatment
vargrind <- All[c(1:60, 241:360), ]
str(vargrind)
vargrind$Site <- as.factor(vargrind$Site)
vargrind$Procedure <- as.factor(vargrind$Procedure)
vargrind$RepID <- as.factor(vargrind$RepID)
str(vargrind)

#Total C using mixed model
Modelg <- lmer(logTC~Site*Procedure + (1|RepID), data = vargrind)
plot(Modelg)
qqnorm(resid(Modelg))
qqline(resid(Modelg), col="black")
anova(Modelg, ddf="Kenward-Roger")
emmeans(Modelg, pairwise ~ Procedure)
emmeans(Modelg, pairwise ~ Procedure|Site)

#Inorganic C using mixed model
#subset for soils with IC
grindICfull <- subset(vargrind, Fizz == "P")
str(grindICfull)
ModelICg <- lmer(logIC~Site*Procedure + (1|RepID), data = grindICfull)
plot(ModelICg)
qqnorm(resid(ModelICg))
qqline(resid(ModelICg), col="black")
anova(ModelICg, ddf="Kenward-Roger")
#pairwise comparisons
emmeans(ModelICg, pairwise ~ Procedure)
emmeans(ModelICg, pairwise ~ Procedure|Site)

#Organic C using mixed model
ModelOCg <- lmer(logOC~Site*Procedure + (1|RepID), data = vargrind)
plot(ModelOCg)
qqnorm(resid(ModelOCg))
qqline(resid(ModelOCg), col="black")
anova(ModelOCg, ddf="Kenward-Roger")
emmeans(ModelOCg, pairwise ~ Procedure)
emmeans(ModelOCg, pairwise ~ Procedure|Site)

#Boxplot figures for % TC and SOC (swapping out variables) for fine grinding procedures

#to display with 2 different y-axis scales
vargrindlowC <- All[c(11:35, 41:45, 251:275,281:285, 311:335, 341:345), ]
vargrindhighC <- All[c(1:10,36:40,46:60,241:250, 276:280,
                       286:310, 336:340, 346:360), ]
#arrange sites in ascending order of % total C
vargrindlowC$Site <- factor(vargrindlowC$Site, levels = c('F','E', 'C', 'G', 'I','D'))
vargrindhighC$Site <- factor(vargrindhighC$Site, levels = c('K','L','B','J','A','H'))

plotlgTC <-ggplot(data=vargrindlowC, aes(x=Site, y=pct_TC, colour=Procedure)) + 
  geom_boxplot()+  
  scale_color_manual("Procedure", values=c("P0" = "#332288","P4" = "#ddcc77", "P5"="#aa4499"))+
  ylim(.7,1.8)+
  ylab(bquote('% TC'))+
  xlab(bquote('Soil'))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=14), 
        strip.text.x = element_text(size = 14,face="bold"))

plotlgTC

plothgTC <-ggplot(data=vargrindhighC, aes(x=Site, y=pct_TC, colour=Procedure)) + 
  geom_boxplot()+
  scale_color_manual("Procedure", values=c("P0" = "#332288","P4" = "#ddcc77", "P5"="#aa4499"))+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  ylab(bquote('% TC'))+
  xlab(bquote('Soil'))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=14), 
        strip.text.x = element_text(size = 14,face="bold")) 
plothgTC

#creating one figure
TCg <- plotlgTC + plothgTC +
  plot_layout(ncol=2, guides="collect")
TCg 
fTCg  <- TCg+plot_annotation(tag_levels = list(c("(a)", "(b)")))
fTCg

ggsave("TC_grind.png", dpi= 300, width = 9, height = 6,TCg )

#Boxplot figure for % SIC for fine grinding procedures
#arranging sites in ascending order of % SIC
grindICfull$Site <- factor(grindICfull$Site, levels = c('D','K', 'L', 'B', 'A','H'))
plotlgIC <-ggplot(data=grindICfull, aes(x=Site, y=pct_SIC, colour=Procedure)) + 
  geom_boxplot()+
  scale_color_manual("Procedure", values=c("P0" = "#332288","P4" = "#ddcc77", "P5"="#aa4499"))+
  ylab(bquote('% SIC'))+
  xlab(bquote('Soil'))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=14), 
        strip.text.x = element_text(size = 14,face="bold")) 

plotlgIC
ggsave("gIC_grind.png", dpi= 300, width = 9, height = 6, plotlgIC)

#Anova and pairwise comparisons effect of drying on % total C, inorganic C, and organic C
#subset for drying treatments
varheat <- All[c(1:60, 361:480), ]
str(varheat)
varheat$Site <- as.factor(varheat$Site)
varheat$Procedure <- as.factor(varheat$Procedure)
varheat$RepID <- as.factor(varheat$RepID)

#Total C using mixed model
Modelh <- lmer(logTC~ Site*Procedure + (1|RepID), data = varheat)
plot(Modelh)
qqnorm(resid(Modelh))
qqline(resid(Modelh), col="black")
anova(Modelh, ddf="Kenward-Roger")
emmeans(Modelh, pairwise ~ Procedure)
emmeans(Modelh, pairwise ~ Procedure|Site)

#Inorganic C using mixed model
#subset for soils for inorganic C
heatICfull <- subset(varheat, Fizz == "P")

ModelICh <- lmer(logIC~Site*Procedure + (1|RepID), data = heatICfull)
plot(ModelICh)
qqnorm(resid(ModelICh))
qqline(resid(ModelICh), col="black")
anova(ModelICh, ddf="Kenward-Roger")
emmeans(ModelICh, pairwise ~ Procedure)
emmeans(ModelICh, pairwise ~ Procedure|Site)

#Organic C using mixed model
ModelOCh <- lmer(logOC~Site*Procedure + (1|RepID), data = varheat)
plot(ModelOCh)
qqnorm(resid(ModelOCh))
qqline(resid(ModelOCh), col="black")
anova(ModelOCh, ddf="Kenward-Roger")
emmeans(ModelOCh, pairwise ~ Procedure)
emmeans(ModelOCh, pairwise ~ Procedure|Site)

#Boxplot figures for % TC and SOC (swapping out variables) for fine grinding procedures
varheat$Site <- factor(varheat$Site, levels = c('F','E', 'C', 'G', 'I', 'D','K','L','B','J','A','H'))
#to display with 2 different y-axis scales
varheatlowC <- All[c(11:35, 41:45, 371:395, 401:405, 431:455, 461:465), ]
varheathighC <- All[c(1:10,36:40,46:60,361:370,396:400,406:430,456:460,466:480), ]
varheatlowC$Site <- factor(varheatlowC$Site, levels = c('F','E', 'C', 'G', 'I','D'))
varheathighC$Site <- factor(varheathighC$Site, levels = c('K','L','B','J','A','H'))

plotlhTC <-ggplot(data=varheatlowC, aes(x=Site, y=pct_TC, colour=Procedure)) + 
  geom_boxplot()+ 
  scale_color_manual("Procedure", values=c("P0" = "#332288", "P6"="#cc6677", "P7"="#882255"))+
  ylab(bquote('% TC'))+
  xlab(bquote('Soil'))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=14), 
        strip.text.x = element_text(size = 14,face="bold")) 

plotlhTC

plothhTC <-ggplot(data=varheathighC, aes(x=Site, y=pct_TC, colour=Procedure)) + 
  geom_boxplot()+ 
  scale_color_manual("Procedure", values=c("P0" = "#332288", "P6"="#cc6677", "P7"="#882255"))+
  scale_y_continuous(labels = label_number(accuracy = 0.1))+
  ylab(bquote('% TC'))+
  xlab(bquote('Soil'))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=14), 
        strip.text.x = element_text(size = 14,face="bold")) 
plothhTC

TCh <- plotlhTC + plothhTC +
  plot_layout(ncol=2, guides="collect")
TCh 
fTCh  <- TCh+plot_annotation(tag_levels = list(c("(a)", "(b)")))
fTCh
ggsave("hTC_heat.png", dpi= 300, width = 9, height = 6, fTCh)

#Boxplot figure for % SIC for drying procedures
plotlhIC <-ggplot(data=heatICfull, aes(x=Site, y=pct_SIC, colour=Procedure)) + 
  geom_boxplot()+ 
  scale_color_manual("Procedure", values=c("P0" = "#332288", "P6"="#cc6677", "P7"="#882255"))+
  ylab(bquote('% SIC'))+
  xlab(bquote('Soil'))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=14), 
        strip.text.x = element_text(size = 14,face="bold")) 

plotlhIC
ggsave("hIC_heat.png", dpi= 300, width = 9, height = 6, plotlhIC)

#Coefficient of variance (CV) analyses

#Total C
#Average % TC for each site*procedure combination
TCsum <- Goldv %>%
  group_by(Site,Procedure) %>%
  summarise(n = n(),
            mean = mean(pct_TC),
            sd = sd(pct_TC),
            se = sd/sqrt(n))

#calculating CV
TCsum$cv_TC <- (TCsum$sd/TCsum$mean)*100
#log transformation
TCsum$cv_TClog <- log(TCsum$cv_TC)
#simple linear model
CVTC <- lm(cv_TClog~Procedure, data=TCsum)
par(mfrow =c(1,2))
plot(CVTC, which =c(1,2))
anova(CVTC)
#pairwise comparison
emmeans(CVTC, pairwise ~ Procedure)

#Inorganic C
#Average % SIC for each site*procedure combination
ICsum <- GoldvICfull %>%
  group_by(Site,Procedure) %>%
  summarise(n = n(),
            mean = mean(pct_SIC),
            sd = sd(pct_SIC),
            se = sd/sqrt(n))

#calculating cv
ICsum$cv_IC <- (ICsum$sd/ICsum$mean)*100
#log transformation
ICsum$cv_IClog <- log(ICsum$cv_IC)
#simple linear model
CVIC <- lm(cv_IClog~Procedure, data=ICsum)
par(mfrow =c(1,2))
plot(CVIC, which =c(1,2))
anova(CVIC)
#pairwise comparison
emmeans(CVIC, pairwise ~ Procedure)

#Organic C
#Average % SOC for each site*procedure combination
OCsum <- Goldv %>%
  group_by(Site,Procedure) %>%
  summarise(n = n(),
            mean = mean(pct_SOC),
            sd = sd(pct_SOC),
            se = sd/sqrt(n))

#Calculating cv
OCsum$cv_OC <- (OCsum$sd/OCsum$mean)*100
#log transformation
OCsum$cv_OClog <- log(OCsum$cv_OC)
#simple linear model
CVOC <- lm(cv_OClog~Procedure, data=OCsum)
par(mfrow =c(1,2))
plot(CVOC, which =c(1,2))
anova(CVOC)
#pairwise comparison
emmeans(CVOC, pairwise ~ Procedure)

#Figure for processing time for each sieving procedure (P0-P3)
#averaging time for each site*procedure combo
Timesum <- Goldv %>%
  group_by(Site,Procedure) %>%
  summarise(n = n(),
            mean = mean(Time),
            sd = sd(Time),
            se = sd/sqrt(n))
#arranging soils in ascending order of % total C 
Timesum$Site <- factor(Timesum$Site, levels = c('F','E', 'C', 'G', 'I', 'D','K','L','B','J','A','H'))


Timeplot <- ggplot(data = Timesum, aes(x=Procedure, y = mean)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  geom_errorbar(aes(x=Procedure, ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9))+
  facet_wrap(~Site, nrow=2) +
  ylab(bquote('Seconds'~(~ gsoil^-1*''))) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size=14), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=10), 
        strip.text.x = element_text(size = 12,face="bold"),
        axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
Timeplot
