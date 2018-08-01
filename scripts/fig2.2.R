
# Define colourblind-friendly palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
               "#0072B2", "#D55E00", "#CC79A7")

# Load libraries
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)

# Assign date
date<-Sys.Date()

cbPalette <- c("#009E73","#F0E442", "#0072B2", "#D55E00", "#CC79A7",
               "#E69F00", "#56B4E9")

text_size <- 8
title_size <- 10

# Read in data
load("data/ch2/sampleSizes_day_2016-05-27.Rds")
load("data/ch2/dayDat_2016-06-08.Rds")

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

p1 <-
ggplot(dayDat,aes(x=LUT,y=mean_temp))+
    geom_jitter(aes(colour=forest_stratum,
                    shape=season,
                    alpha = LUT))+
    geom_boxplot(width=0.3,
                alpha=0.8)+
    theme_classic(base_size = 10)+
    ylab(expression(paste("Temperature (",degree*C,")",sep="")))+
    theme(axis.title.x = element_blank(),
          axis.text.x=element_text(size= title_size,
                                   margin=margin(t = 5,
                                                 unit = "pt"),
                                   colour="black"),
          axis.text.y=element_text(size= text_size,
                                   margin=margin(r = 5,
                                                 unit = "pt"),
                                   colour="black"),
          axis.title.y = element_text(size= title_size,
                                      margin=margin(r = 5,
                                                    unit = "pt")),
          axis.line.x = element_line(colour = 'black', 
                                     size=0.5, 
                                     linetype='solid'),
          axis.line.y = element_line(colour = 'black', 
                                     size=0.5, 
                                     linetype='solid'),
          legend.position=c(0.2,0.95),
          # legend.text.align	= 0,
          legend.text=element_text(size= text_size),
          legend.title=element_blank(),
          legend.key=element_blank(),
          legend.spacing=unit(0,"cm"),
          legend.box = "horizontal",
          legend.box.just = "left",
          panel.grid=element_blank())+
    scale_y_continuous(breaks=seq(10,50,5))+
    scale_x_discrete(labels=labs)+
    scale_colour_manual(values=c("#D55E00","#0072B2"),
                        guide=guide_legend(order=1)) +
    scale_alpha_manual(values = c(rep(0.5, 3),
                                  rep(0.8, 2)), 
                       guide = FALSE)

saveRDS(p1, file = "figs/fig2.2.RdS")
