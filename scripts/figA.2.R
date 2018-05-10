rm(list=ls())

user<-Sys.info()[7]

#Define working directory
baseDir<-file.path("C:/Users",user,"Google Drive/PhD/Ch2/Data")

# Define colourblind-friendly palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
               "#0072B2", "#D55E00", "#CC79A7")
# Define font sizes
lab_size <- 7
text_size <- 8
title_size <- 10

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
load(file.path(baseDir,"modelOutputs_night_2016-05-26.Rd"))
load(file.path(baseDir,"sampleSizes_night_2016-05-25.Rds"))

projections<-read.csv(file.path(baseDir,"ipcc_projections.csv"))
projections<-projections[projections$region=="tropics" &
                           projections$RCP %in% c("RCP2.6","RCP8.5"),]
projections$posCI<-projections$mean_tempChange+
  projections$range_tempChange_0.95
projections$negCI<-projections$mean_tempChange-
  projections$sd_tempChange


max(sumStats$mean_temp_st)
# [1] 0
min(sumStats$mean_temp_st)
# [1] -0.8545481

sumStats<-sumStats[sumStats$LUT!="Primary vegetation",]
sumLUT<-sumLUT[sumLUT$LUT!="Primary vegetation",]

sumStats$LUT <- factor(sumStats$LUT,
                       levels = c("Degraded vegetation",
                                  "Plantation"))
sumLUT$LUT <- factor(sumLUT$LUT,
                     levels = c("Degraded vegetation",
                                "Plantation"),
                     labels = c("Degraded forest",
                                "Plantation"))

# Define x-axis labels
labs<-paste(levels(sumLUT$LUT),
            "\n(",sumLUT$N_sources," studies)",sep="")

p1<-ggplot(sumStats, aes(x=LUT, y=mean_temp_st))+
  geom_point(position = position_dodge(width = 0.4),size=2)+
  geom_errorbar(aes(ymin = mean_temp_st-CI, 
                    ymax = mean_temp_st+CI),width=0.1,
                position = position_dodge(width = 0.4))+
  theme_bw()+
  geom_text(aes(x=1.5,y=2,label="n.s."),size=3,colour="black")+
  geom_hline(yintercept =0,linetype="dashed",colour="black")+
  ylab(expression(paste("Temperature difference (",degree*C,")",sep="")))+
  theme(axis.title.x = element_blank(),
        axis.text.x=element_text(size= title_size,
                                 margin=margin(t = 5, unit = "pt"), 
                                 colour = "black"),
        axis.text.y=element_text(size= text_size,
                                 margin=margin(r = 5, unit = "pt")),
        axis.title.y = element_text(size= title_size,
                                    vjust=1),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
        legend.position=c(0.1,0.85),
        legend.text.align	= 0,
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.box = "horizontal",
        legend.box.just = "left",
        panel.grid=element_blank())+
  scale_x_discrete(labels=labs)+
  scale_y_continuous(breaks=seq(-10,20,1))

saveRDS(p1, file = "figs/figA.4.Rds")

