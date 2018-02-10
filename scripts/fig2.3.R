rm(list=ls())

user<-Sys.info()[7]

#Define working directory
baseDir<-file.path("C:/Users",user,"Google Drive/PhD/Ch2/Data")

# Define colourblind-friendly palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
               "#0072B2", "#D55E00", "#CC79A7")
text_size <- 8
title_size <- 10
lab_size <- 7

# Load libraries
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(cowplot)

# Assign date
date<-Sys.Date()

# Read in data
load(file.path(baseDir,"modelOutputs_day_2017-05-19.Rd"))
load(file.path(baseDir,"sampleSizes_day_2016-05-27.Rds"))

projections<-read.csv(file.path(baseDir,"ipcc_projections.csv"))
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
    geom_errorbar(aes(ymin = plo, 
                      ymax = phi),width=0.1,
                  position = position_dodge(width = 0.4))+
    # annotate("text", x = 4.5, y = 15.5, label = "(a)",size=3.5)+
    theme_classic()+
    ylab(expression(paste("Temperature difference (",degree*C,")",sep="")))+
    theme(axis.title.x = element_blank(),
          # axis.text.x=element_blank(),
          axis.text.x=element_text(size=0.05,
                                   margin=margin(t = 5, unit = "pt"),
                                   colour="white"),
          axis.text.y=element_text(size= text_size,
                                   margin=margin(r = 5, unit = "pt"),
                                   colour="black"),
          axis.title.y = element_text(size= title_size,vjust=1), 
          axis.line.x = element_line(colour = 'black', 
                                     size=0.5, 
                                     linetype='solid'),
          axis.line.y = element_line(colour = 'black', 
                                     size=0.5, 
                                     linetype='solid'),
          legend.position=c(0.2,0.85),
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
    geom_errorbar(aes(ymin = plo, 
                      ymax = phi),width=0.1,
                  position = position_dodge(width = 0.4))+
    # annotate("text", x = 4.5, y = 15.5, label = "(b)",size=3.5)+
    theme_classic()+
    ylab(expression(paste("Temperature difference (",degree*C,")",sep="")))+
    theme(axis.title.x = element_blank(),
          axis.text.x=element_text(size= title_size,
                                   margin=margin(t = 5, unit = "pt"),colour="black"),
          axis.text.y=element_text(size= text_size,margin=margin(r = 5, unit = "pt"),colour="black"),
          axis.title.y = element_text(size= title_size,vjust=1),
          axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
          axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
          legend.position=c(0.1,0.8),
          legend.text.align	= 0,
          legend.text = element_text(size= text_size),
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

p1 <- p1 + theme(axis.title.y = element_blank()) 
p2<- p2 + theme(axis.title.y = element_blank())

p3<- ggdraw() +
    draw_plot(p1, x = 0.1, y = 0.5, width = 0.9, height = 0.5) +
    draw_plot(p2, x = 0.1, y = 0, width = 0.9, height = 0.5) +
    draw_text(paste("Temperature difference (", "\U00B0", "C)",sep = ""),
              x = 0.08, y= 0.5,size = title_size, angle=90) +
    draw_plot_label(label = c("(a)", "(b)"),
                    x = c(0.95, 0.95),
                    y = c(0.99, 0.5),
                    size = lab_size)
# ggsave(p3, filename = "figs/test.png", dpi = 200,
#        width = 16.6/2.54, height = 10/2.54, units = "in") 

saveRDS(p3, file = "figs/fig2.3.RdS")
