library(tidyverse)
library(ggplot2)
library(ggpubr)

#data set structured for regression figure
ftirq <- read.csv("") #set path to location of file FTIRTEST.csv

F = as.formula(y~x)

#Total C
#Ordering particle size in descending order
Grind3 <- factor(ftirq$Grind_type, 
                 level = c('<2000','<250', '<180', '<125'))
tcftir <- ggplot(ftirq,aes(x=TC_m,y=TC_FTIR, color=Grind3))+ 
  geom_point()+ 
  scale_color_manual("Grind", values=c("<2000" = "olivedrab" , "<250" = "orange4", "<180"="violetred", "<125" = "sienna1"))+
  scale_fill_manual("Grind", values=c("<2000" = "olivedrab" , "<250" = "orange4", "<180"="violetred", "<125" = "sienna1"))+
  xlab(bquote('observed % TC'))+
  ylab(bquote('FTIR predicted % TC'))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=14), 
        strip.text.x = element_text(size = 14,face="bold"))



tcftir

#Plotting multiple regression lines and equations
tcftir1 <- tcftir+geom_smooth(method=lm,se=FALSE,fullrange=TRUE, linewidth=.75)+
  stat_regline_equation(
    label.x = c(2,2,2,2), label.y = c(1.5,1.1,0.7,0.3),
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula=F,size=4)
tcftir1

#getting p-value for significance
f <- lm(TC_FTIR ~ TC_m, data=ftirq[ftirq$Grind_type=="<2000",])
summary(f)

f1 <- lm(TC_FTIR ~ TC_m, data=ftirq[ftirq$Grind_type=="<250",])
summary(f1)

f2 <- lm(TC_FTIR ~ TC_m, data=ftirq[ftirq$Grind_type=="<180",])
summary(f2)

f3 <- lm(TC_FTIR ~ TC_m, data=ftirq[ftirq$Grind_type=="<125",])
summary(f3)


#inorganic C
#subset for soils with inorganic C
IC <- subset(ftirq, Fizz == "P" )
#ordering particle size in descending order
Grind2  <- factor(IC$Grind_type, 
                  level = c('<2000','<250', '<180', '<125'))

icftir <- ggplot(IC,aes(x=IC_m,y=IC_FTIR, color=Grind2))+ 
  geom_point()+ 
  scale_color_manual("Grind", values=c("<2000" = "olivedrab" , "<250" = "orange4", "<180"="violetred", "<125" = "sienna1"))+
  scale_fill_manual("Grind", values=c("<2000" = "olivedrab" , "<250" = "orange4", "<180"="violetred", "<125" = "sienna1"))+
  xlab(bquote('observed % SIC'))+
  ylab(bquote('FTIR predicted % SIC'))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=14), 
        strip.text.x = element_text(size = 14,face="bold"))

icftir

#Plotting multiple Regression Lines 
icftir1 <- icftir+geom_smooth(method=lm,se=FALSE,fullrange=TRUE, linewidth=.75)+
  stat_regline_equation(
    label.x = c(1,1,1,1), label.y = c(.75,.5,.25,0),
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula=F,size=4)
icftir1

#getting p-value for significance
f4 <- lm(IC_FTIR ~ IC_m, data=ftirq[ftirq$Grind_type=="<2000",])
summary(f4)

f5 <- lm(IC_FTIR ~ IC_m, data=ftirq[ftirq$Grind_type=="<250",])
summary(f5)

f6 <- lm(IC_FTIR ~ IC_m, data=ftirq[ftirq$Grind_type=="<180",])
summary(f6)

f7 <- lm(IC_FTIR ~ IC_m, data=ftirq[ftirq$Grind_type=="<125",])
summary(f7)

#organic C
ocftir <- ggplot(ftirq,aes(x=OC_m,y=OC_FTIR, color=Grind3))+ 
  geom_point()+ 
  scale_color_manual("Grind", values=c("<2000" = "olivedrab" , "<250" = "orange4", "<180"="violetred", "<125" = "sienna1"))+
  scale_fill_manual("Grind", values=c("<2000" = "olivedrab" , "<250" = "orange4", "<180"="violetred", "<125" = "sienna1"))+
  xlab(bquote('observed % SOC'))+
  ylab(bquote('FTIR predicted % SOC'))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=14), 
        strip.text.x = element_text(size = 14,face="bold")) 

ocftir

#Plotting multiple Regression Lines 
ocftir1 <- ocftir+geom_smooth(method=lm,se=FALSE,fullrange=TRUE, linewidth=.75)+
  stat_regline_equation(
    label.x = c(1.5,1.5,1.5,1), label.y = c(1.1,0.8,0.5,0.2),
    aes(label =  paste(..eq.label.., ..rr.label.., sep = "~~~~")),
    formula=F,size=4)
ocftir1

#getting p-value for significance
f8 <- lm(OC_FTIR ~ OC_m, data=ftirq[ftirq$Grind_type=="<2000",])
summary(f8)

f9 <- lm(OC_FTIR ~ OC_m, data=ftirq[ftirq$Grind_type=="<250",])
summary(f9)

f10  <- lm(OC_FTIR ~ OC_m, data=ftirq[ftirq$Grind_type=="<180",])
summary(f10)

f11 <- lm(OC_FTIR ~ OC_m, data=ftirq[ftirq$Grind_type=="<125",])
summary(f11)

#creating one figure
library(patchwork)
ftirg <- tcftir1 + icftir1 + ocftir1+
  plot_layout(ncol=3, guides="collect")
ftirg
#labeling panels
fftirg  <- ftirg+plot_annotation(tag_levels = list(c("(a)", "(b)", "(c)")))
fftirg

