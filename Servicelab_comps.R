#csv with external, service lab data
ExC <- read.csv("") # set path to location of file exlabs.csv

library(tidyverse)
library(ggplot2)

#Figures for total C, inorganic and organic C

#setting colorblind palette
cbPalette <- c("#999999","#000000", "#E69F00", "#56B4E9", "#009E73",
               "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

#dotplot for TC without a legend
TCexsumdot <- ggplot(data = ExC)+(aes(x=Site1, y = pct_TC, colour = Lab, shape=sample_state)) +
  geom_jitter(size = 4, width=0.1)+ 
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ylab(bquote('% TC')) +
  xlab(bquote('Soil'))+
  scale_color_manual(values = cbPalette)+
  scale_shape_manual(values=c(16, 17))+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=14), 
        strip.text.x = element_text(size = 14,face="bold"))+
  theme(legend.position = "none")
TCexsumdot


#dotplot for OC without a legend
OCexsumdot <- ggplot(data = ExC)+(aes(x=Site1, y = pct_SOC, colour = Lab, shape=sample_state)) +
  geom_jitter(size = 4, width=0.1)+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ylab(bquote('% SOC')) +
  xlab(bquote('Soil'))+
  scale_color_manual(values = cbPalette)+
  scale_shape_manual(values=c(16, 17))+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=14), 
        strip.text.x = element_text(size = 14,face="bold"))+
  theme(legend.position = "none")
OCexsumdot

#dotplot for IC with legend
ICexsumdot <- ggplot(data = ExC)+(aes(x=Site1, y = pct_SIC, colour = Lab, shape=sample_state)) +
  geom_jitter(size = 4, width=0.1)+
  scale_color_manual(values = cbPalette)+
  scale_shape_manual(values=c(16, 17))+
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  ylab(bquote('% SIC')) +
  xlab(bquote('Soil'))+
  theme(axis.text=element_text(size=14), axis.title=element_text(size=14), 
        legend.title = element_text(size=14), 
        legend.text = element_text(size=14),
        strip.text.x = element_text(size = 14,face="bold"))
        
ICexsumdot

#creating one figure
library('patchwork')
adot <- TCexsumdot + ICexsumdot + OCexsumdot+
  plot_layout(ncol=3, guides="collect")
#adding panel labels
fadot <- adot+plot_annotation(tag_levels = list(c("(a)", "(b)", "(c)")))
fadot

ggsave("Allexlabdot2.png", dpi= 300, width = 12, height = 9, fadot)

#Finding NAPT acceptable range based on median absolute deviation

#TC
MAD_TC <- ExC %>%
  group_by (Site1) %>%
  summarise(n=n(),
            med = median(pct_TC), # median
            MAD= mad(pct_TC)) # median absolute deviation
MAD_TC$Llimit <- MAD_TC$med - (MAD_TC$MAD * 2.5) #lower limit acceptable range
MAD_TC$Ulimit <- MAD_TC$med + (MAD_TC$MAD * 2.5) #upper limit acceptable range

#SIC
MAD_SIC <- ExC %>%
  group_by (Site1) %>%
  summarise(n=n(),
            med = median(pct_SIC),
            MAD= mad(pct_SIC))
MAD_SIC$Llimit <- MAD_SIC$med - (MAD_SIC$MAD * 2.5)
MAD_SIC$Ulimit <- MAD_SIC$med + (MAD_SIC$MAD * 2.5)

#SOC
MAD_SOC <- ExC %>%
  group_by (Site1) %>%
  summarise(n=n(),
            med = median(pct_SOC),
            MAD= mad(pct_SIC))
MAD_SOC$Llimit <- MAD_SOC$med - (MAD_SOC$MAD * 2.5)
MAD_SOC$Ulimit <- MAD_SOC$med + (MAD_SOC$MAD * 2.5)


