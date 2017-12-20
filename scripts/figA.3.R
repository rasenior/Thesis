rm(list=ls())

user<-Sys.info()[7]

#Define working directory
baseDir<-file.path("C:/Users",user,"Google Drive/PhD/Ch2/Data")
# Set working directory
setwd(baseDir)

# Define colourblind-friendly palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
               "#0072B2", "#D55E00", "#CC79A7")

# Load libraries
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)

# ggplot(mtcars, aes(disp, mpg)) + geom_point() + 
  # theme(axis.ticks.length=unit(-0.25, "cm"), axis.ticks.margin=unit(0.5, "cm"))

# Assign date
date<-Sys.Date()

# Read in data
load("modelOutputs_day_sampleTest2017-05-18.Rd")
load("sampleSizes_day_2016-05-27.Rds")

projections<-read.csv("../Data/ipcc_projections.csv")
projections<-projections[projections$region=="tropics" &
                           projections$RCP %in% c("RCP2.6","RCP8.5"),]
projections$posCI<-projections$mean_tempChange+
  projections$range_tempChange_0.95
projections$negCI<-projections$mean_tempChange-
  projections$sd_tempChange


max(sumStats$mean_temp_st)
# [1] 13.60875
min(sumStats$mean_temp_st)
# [1] 0

sumStats<-sumStats[sumStats$LUT!="Primary vegetation",]
sumLUT<-sumLUT[sumLUT$LUT!="Primary vegetation",]

sumStats$LUT <- factor(sumStats$LUT,
                       levels = c("Degraded vegetation",
                                  "Plantation","Pasture","Cropland"))
sumLUT$LUT <- factor(sumLUT$LUT,
                     levels = c("Degraded vegetation",
                                "Plantation","Pasture","Cropland"),
                     labels = c("Degraded forest",
                                "Plantation","Pasture","Cropland"))

# Define x-axis labels
labs<-paste(levels(sumLUT$LUT),
            "\n(",sumLUT$N_sources," studies)",sep="")

### Forest stratum interaction first
height<-sumStats[sumStats$season=="Dry season",]

p1<-ggplot(height, aes(x=LUT, y=mean_temp_st,
                       shape=forest_stratum))+
  geom_point(position = position_dodge(width = 0.4),size=2)+
  geom_hline(data=projections,
             aes(yintercept=mean_tempChange,
                 colour=RCP),
             alpha=0.6)+
  geom_rect(data=projections,
            aes(ymin=range_tempChange_0.05,
                ymax=range_tempChange_0.95,
                xmin=0.4,xmax=4.6,
                fill=RCP),
            alpha=0.2,inherit.aes = FALSE)+
  geom_hline(yintercept =0,linetype="dashed",colour="black")+
  geom_point(data=height,aes(x=LUT, y=mean_temp_st,
                             shape=forest_stratum),
             position = position_dodge(width = 0.4),size=2)+
  geom_errorbar(aes(ymin = mean_temp_st-CI, 
                    ymax = mean_temp_st+CI),width=0.1,
                position = position_dodge(width = 0.4))+
  annotate("text", x = 4.5, y = 15.5, label = "(a)",size=3.5)+
  theme_classic()+
  ylab(expression(paste("Temperature difference (",degree*C,")",sep="")))+
  theme(axis.title.x = element_blank(),
        # axis.text.x=element_blank(),
        axis.text.x=element_text(size=0.05,margin=margin(t = 5, unit = "pt"),
                                 colour="white"),
        axis.text.y=element_text(size=8,margin=margin(r = 5, unit = "pt"),colour="black"),
        axis.title.y = element_text(size=8,vjust=1), 
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
        legend.position=c(0.185,0.85),
        legend.text.align	= 0,
        legend.text = element_text(size=8),
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.box = "horizontal",
        legend.box.just = "left",
        panel.grid=element_blank(),
        panel.border = element_blank() ,
        plot.margin = unit(c(0.5,0.5,0.5,0.5), "lines"),
        panel.background = element_blank())+
  scale_y_continuous(breaks=seq(-4,20,2))+
  scale_x_discrete(labels=labs)+
  scale_shape_discrete(guide=guide_legend(order=1))+
  scale_fill_manual(values=cbPalette[c(4,7)],
                    guide=guide_legend(order=2))+
  scale_colour_manual(values=cbPalette[c(4,7)],
                      guide=guide_legend(order=2))
p1

# ggsave(filename = file.path("..","Figures/Fig3.png"),
#        plot = p1,dpi = 800,width=16.6,height=8,units="cm")
# ggsave(filename = file.path("..","Figures/Fig3.pdf"),
#        plot = p1,dpi = 800,width=16.6,height=8,units="cm")

### Season next
season<-sumStats[sumStats$forest_stratum=="Above-ground",]

p2<-ggplot(season, aes(x=LUT, y=mean_temp_st,
                       shape=season))+
  geom_point(position = position_dodge(width = 0.4),size=2)+
  geom_hline(data=projections,
             aes(yintercept=mean_tempChange,
                 colour=RCP),
             alpha=0.6)+
  geom_rect(data=projections,
            aes(ymin=range_tempChange_0.05,
                ymax=range_tempChange_0.95,
                xmin=0.4,xmax=4.6,
                fill=RCP),
            alpha=0.2,inherit.aes = FALSE)+
  geom_hline(yintercept =0,linetype="dashed",colour="black")+
  geom_point(data=season,aes(x=LUT, y=mean_temp_st,
                             shape=season),
             position = position_dodge(width = 0.4),size=2)+
  geom_errorbar(aes(ymin = mean_temp_st-CI, 
                    ymax = mean_temp_st+CI),width=0.1,
                position = position_dodge(width = 0.4))+
  annotate("text", x = 4.5, y = 15.5, label = "(b)",size=3.5)+
  theme_classic()+
  ylab(expression(paste("Temperature difference (",degree*C,")",sep="")))+
  theme(axis.title.x = element_blank(),
        axis.text.x=element_text(size=8,margin=margin(t = 5, unit = "pt"),colour="black"),
        axis.text.y=element_text(size=8,margin=margin(r = 5, unit = "pt"),colour="black"),
        axis.title.y = element_text(size=8,vjust=1),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
        legend.position=c(0.094,0.8),
        legend.text.align	= 0,
        legend.text = element_text(size=8),
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.box = "horizontal",
        legend.box.just = "left",
        plot.margin = unit(c(-0.3,0.5,0.5,0.5), "lines"),
        panel.grid=element_blank())+
  scale_y_continuous(breaks=seq(-4,20,2))+
  scale_x_discrete(labels=labs)+
  scale_fill_manual(values=cbPalette[c(4,7)],
                    guide=guide_legend(order=2))+
  scale_colour_manual(values=cbPalette[c(4,7)],
                      guide=guide_legend(order=2))+
  scale_shape_discrete(guide=guide_legend(order=1))+
  guides(fill=FALSE,colour=FALSE)
p2

# ggsave(filename = file.path("..","Figures/Fig4.png"),
#        plot = p2,dpi = 800,width=16.6,height=8,units="cm")
# ggsave(filename = file.path("..","Figures/Fig4.pdf"),
#        plot = p2,dpi = 800,width=16.6,height=8,units="cm")

p3<-grid.arrange(arrangeGrob(p1 + theme(axis.title.y = element_blank()), 
                             p2 + theme(axis.title.y = element_blank()), 
                             nrow = 2,
                             left = textGrob(expression(paste("Temperature difference (",degree*C,")",sep="")),
                                             rot = 90,vjust=1,
                                             gp=gpar(fontsize=10))))

saveRDS(p3, file = "../../Thesis/figs/figA.3.Rds")

# saveRDS(p3,"../Thesis version/Figs/FigS3.Rds")
# 
# ggsave(filename = "../Ecology and Evolution/Revisions/Figures/ESM_2.png",
#        plot = p3,dpi = 800,width=16.6,height=8,units="cm")
ggsave(filename = "../../Thesis/figs/figA.3.pdf",
			 plot = p3,dpi = 800,width=16.6,height=10,units="cm")

