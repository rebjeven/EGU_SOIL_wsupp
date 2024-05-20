library(tidyverse)
library(ggplot2)
library(ggpubr)

#Regressions for all quantification procedures vs. the P0 quantification procedures

#data set structured for regression figures
P0r <- read.csv("") #set path to location of file P0_reg.csv

#Regressions for inorganic C
F = as.formula(y~x)
#subset for soils with inorganic C procedures that quantified % IC
ICNAp <- subset(P0r, !is.na(IC) & Fizz == "P" )

legend_title <-"Methods" 
ggplt <- ggplot(ICNAp,aes(x=P0_IC,y=IC, color=SIC_Q))+ 
  geom_point()+ 
  scale_color_manual(legend_title, values=c("FTIR" = "#E69F00", "EA-AF"="#007282"))+
  scale_fill_manual(legend_title, values=c("FTIR" = "#E69F00", "EA-AF"="#007282"))+
  xlab(bquote('% SIC (PT)'))+
  ylab(bquote(' % SIC')) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14), 
        legend.position="none", #no legend
        strip.text.x = element_text(size = 14,face="bold"))

ggplt 


#Plotting multiple regression lines and equations
P0rIC <-ggplt+geom_smooth(method=lm,se=FALSE,fullrange=TRUE, linewidth=.75)+
  stat_regline_equation(
    label.x = c(1.1,1.1), label.y = c(0.5,0.25),
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula=F,size=4)
P0rIC

#getting p-value for significance
mo <- lm(IC ~ P0_IC, data=ICNAp[ICNAp$SIC_Q=="EA-AF",])
summary(mo)

mo1 <- lm(IC ~ P0_IC, data=ICNAp[ICNAp$SIC_Q=="FTIR",])
summary(mo1)

#Regressions for organic C
ggplto <- ggplot(P0r,aes(x=P0_OC,y=OC, color=SOC_Q))+ 
  geom_point()+ 
  scale_color_manual(legend_title, values=c("FTIR" = "#E69F00", "AF"="#007282", "LOI"="#D55E00"))+
  scale_fill_manual(legend_title, values=c("FTIR" = "#E69F00", "AF"="#007282", "LOI"="#D55E00"))+
  xlab(bquote('% SOC (EA-PT)'))+
  ylab(bquote(' % SOC')) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=14), 
        strip.text.x = element_text(size = 14,face="bold")) 

ggplto 

#Plotting multiple regression lines and equations 
P0rOC <-ggplto+geom_smooth(method=lm,se=FALSE,fullrange=TRUE,linewidth=.75)+
  stat_regline_equation(
    label.x = c(1.5,1.5,1.5), label.y = c(1.0,0.75,0.5),
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula=F,size=4)
P0rOC

ggsave("OCFTIR.png", dpi= 300, width = 7, height = 7)

#getting p-value for significance
mo2 <- lm(OC ~ P0_OC, data=OCNAp[OCNAp$SOC_Q=="LOI",])
summary(mo2)

mo3 <- lm(OC ~ P0_OC, data=OCNAp[OCNAp$SOC_Q=="AF",])
summary(mo3)

mo4 <- lm(OC ~ P0_OC, data=OCNAp[OCNAp$SOC_Q=="FTIR",])
summary(mo4)


#Regression for total C
#subset for procedures quantifying % total C
TCNAp <- subset(P0r, !is.na(TC))

ggpltt <- ggplot(TCNAp,aes(x=P0_TC,y=TC, color=TC_Q))+ 
  geom_point()+ 
  scale_color_manual(legend_title, values=c("FTIR" = "#E69F00"))+
  scale_fill_manual(legend_title, values=c("FTIR" = "#E69F00"))+
  xlab(bquote('% TC (EA)'))+
  ylab(bquote('% TC')) +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14), 
        legend.position="none", 
        strip.text.x = element_text(size = 14,face="bold"))

ggpltt 

# Plotting multiple Regression Lines 
P0tc <- ggpltt+geom_smooth(method=lm,se=FALSE,fullrange=TRUE,linewidth=.75)+
  stat_regline_equation(
    label.x = c(2,2), label.y = c(1.5,1.25),
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula=F,size=4)

P0tc

#getting p-value for significance
mo5 <- lm(TC ~ P0_TC, data=TCNAp[TCNAp$TC_Q=="FTIR",])
summary(mo5)

#creating one figure
library(patchwork)
q <- P0tc + P0rIC + P0rOC +
  plot_layout(ncol=3, guides="collect")
q 
fq  <- q+plot_annotation(tag_levels = list(c("(a)", "(b)", "(c)")))
fq
ggsave("fquant.png", dpi= 300, width = 12, height = 9, fq)

#Coefficient of variance for procedures P0-P10 

#Full data set for P0-P10
AllQ <- read.csv("") #set path to location of file Data_quant.csv

#subset for all procedures that quantify % TC
TCsub <- AllQ[c(1:540), ]
#average % TC for each site*procedure combo
TCpSUMall <- TCsub %>%
  group_by(Site, Procedure) %>%
  summarise(n = n(),
            mean = mean(pct_TC),
            sd = sd(pct_TC),
            se = sd/sqrt(n))
#calculating cv  
TCpSUMall$cv <- (TCpSUMall$sd/TCpSUMall$mean)*100
#boxplot for cv % TC
cvTC<- ggplot(TCpSUMall, aes(x=Procedure, y = cv, color=Procedure))+
  geom_boxplot()+
  ylab(bquote('CV % TC'))+
  scale_color_manual("Procedure", values=c("P0" = "#332288", "P1"="#117733", "P2" = "#44aa99","P3"= "#88ccee",
                                           "P4" = "#ddcc77", "P5"="#aa4499", "P6"="#cc6677", "P7"="#882255", 
                                           "P8" = "#E69F00"))+
  
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14), 
        legend.position="none", 
        strip.text.x = element_text(size = 14,face="bold"),
        axis.text.x = element_text(angle=90, vjust=.5, hjust=1))

cvTC

#subset for all procedures that quantify % SIC
ICquant <- AllQ[c(1:600), ]
ICcv <- subset(ICquant, Fizz == "P")
#average % SIC for each site*procedure combo
ICpSUM <- ICcv %>%
  group_by(Site,Procedure) %>%
  summarise(n = n(),
            mean = mean(pct_SIC),
            sd = sd(pct_SIC),
            se = sd/sqrt(n))
#calculating cv
ICpSUM$cv <- (ICpSUM$sd/ICpSUM$mean)*100
#boxplot for cv % SIC
cvIC<- ggplot(ICpSUM, aes(x=Procedure, y = cv, color=Procedure))+
  geom_boxplot()+
  ylab(bquote('CV % SIC')) +
  scale_color_manual("Procedure", values=c("P0" = "#332288", "P1"="#117733", "P2" = "#44aa99","P3"= "#88ccee",
                                           "P4" = "#ddcc77", "P5"="#aa4499", "P6"="#cc6677", "P7"="#882255", 
                                           "P8" = "#E69F00", "P9"="#007282"))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14), 
        legend.position="none", 
        strip.text.x = element_text(size = 14,face="bold"),
        axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
cvIC

#average % SOC for each site*procedure combo
OCSUM <- AllQ %>%
  group_by(Site,Procedure) %>%
  summarise(n = n(),
            mean = mean(pct_SOC),
            sd = sd(pct_SOC),
            se = sd/sqrt(n))
#calculating cv
OCSUM$cv <- (OCSUM$sd/OCSUM$mean)*100
#ordering procedures in ascending order
OCo <- factor(OCSUM$Procedure, 
              level = c('P0', 'P1', 'P2', 'P3', 'P4', 'P5',
                        'P6','P7','P8', 'P9','P10'))
cvOC<- ggplot(OCSUM, aes(x=OCo, y = cv, color=OCo))+
  geom_boxplot()+
  ylab(bquote('CV % SOC')) +
  xlab(bquote('Procedure'))+
  scale_color_manual("Procedure", values=c("P0" = "#332288", "P1"="#117733", "P2" = "#44aa99","P3"= "#88ccee",
                                           "P4" = "#ddcc77", "P5"="#aa4499", "P6"="#cc6677", "P7"="#882255", 
                                           "P8" = "#E69F00", "P9"="#007282", "P10"="#D55E00"))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=14), 
        strip.text.x = element_text(size = 14,face="bold"),
        axis.text.x = element_text(angle=90, vjust=.5, hjust=1))
cvOC

#creating one figure)
cva <- cvTC + cvIC + cvOC+
  plot_layout(ncol=3, guides="collect")
cva
#labeling panels
fcva  <- cva + plot_annotation(tag_levels = list(c("(a)", "(b)", "(c)")))
fcva
ggsave("cv_final.png", dpi= 300, width = 9, height = 6, fcva)
