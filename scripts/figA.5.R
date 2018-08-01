
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

# Assign date
date<-Sys.Date()

cbPalette <- c("#009E73","#F0E442", "#0072B2", "#D55E00", "#CC79A7",
               "#E69F00", "#56B4E9")

# Read in data
load("data/ch2/sampleSizes_day_2016-05-27.Rds")
load("data/ch2/dayDat_2016-06-08.Rds")

# Reduce to unique sites
dayDat<-dayDat[which(!(duplicated(dayDat$SS))),]

# Calculate mean elevation for each study and LUT
elev_sum<-summarise(group_by(dayDat,studyID,LUT),
                    mean_elev=round(mean(elevation),digits = 0))

###############################################################################
###### Look at some exploratory figures
###############################################################################
sumLUT$LUT <- factor(sumLUT$LUT,
                     levels = c("Primary vegetation",
                                "Degraded vegetation",
                                "Plantation","Pasture","Cropland"),
                     labels = c("Primary forest",
                                "Degraded forest",
                                "Plantation","Pasture","Cropland"))

# Define x-axis labels
labs<-paste(levels(sumLUT$LUT),
            "\n(",sumLUT$N_sources," studies)",sep="")


dayDat$alpha<-abs(dayDat$densityProp-1)/3

dayDat$alpha[dayDat$LUT %in% c("Cropland","Pasture")]<-0.6


### Plot by study
labs<-c("PF","DF","Pl","Pa","Cr")
shades<-c("#E0EDD2","#D1ECE4","#E3E2F0","#F7DFCC","#FAEECC")
textCol<-c("#66A61E","#1B9E77","#7570B3","#D95F02","#E6AB02")

# Reorder factor levels
dayDat$study <- factor(dayDat$study,
                       levels = unique(dayDat$study))

# Create data.frame with shading info
shading <- data.frame(min = seq(from = 0.5, 
                                to = max(as.numeric(as.factor(dayDat$LUT))), 
                                by = 1),
                      max = seq(from = 1.5, 
                                to = max(as.numeric(as.factor(dayDat$LUT))) + 0.5, 
                                by = 1),
                      LUT = levels(dayDat$LUT),
                      labs = labs,
                      labPos = 1:5,
                      labCols = textCol)
shading$min[1]<--Inf
shading$max[5]<-Inf



p1<-ggplot()+
      geom_point(data = dayDat,aes(x=LUT,y=elevation),alpha=0)+
      geom_rect(data=shading,
                aes(xmin = min, xmax = max, ymin = -Inf, ymax = Inf,
                    fill = factor(labPos)))+
      stat_summary(data = dayDat,
                   aes(x=LUT,y=elevation,group=studyID),
                   fun.y=mean,geom="line",size=0.3,linetype="dotted") +
      geom_point(data = dayDat,aes(x=LUT,y=elevation,colour=LUT))+
      geom_text(data=shading,
                aes(x = labPos,y = -250, label = labs),
                colour=rep(textCol,25),size=2.5)+
      theme_bw(base_size = 10)+
      ylab("Elevation (m)")+
      theme(axis.title.x = element_blank(),
            axis.text.x=element_blank(),
            axis.text.y=element_text(size= text_size,
                                     margin=margin(r = 5,unit = "pt")),
            axis.title.y = element_text(size= title_size,
                                        margin=margin(r = 5,unit = "pt")),
            axis.ticks.x = element_blank(),
            axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
            strip.text=element_text(size= text_size),
            strip.background=element_rect(fill="white"),
            legend.position="top",
            legend.text=element_text(size= text_size),
            legend.title=element_blank(),
            legend.key=element_blank(),
            legend.spacing=unit(0.1,"cm"),
            legend.box = "horizontal",
            legend.box.just = "left",
            panel.grid=element_blank())+
      scale_y_continuous(breaks=seq(0,2500,500),limits=c(-250,2500))+
      scale_fill_manual(values = shades,guide=FALSE)+
      scale_colour_manual(values=textCol,guide=FALSE)+
      facet_wrap(~studyID)
p1

saveRDS(p1, file = "figs/figA.5.Rds")
