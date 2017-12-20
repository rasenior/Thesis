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

# Assign date
date<-Sys.Date()

cbPalette <- c("#009E73","#F0E442", "#0072B2", "#D55E00", "#CC79A7",
               "#E69F00", "#56B4E9")

# Read in data
load("sampleSizes_day_2016-05-27.Rds")
load("dayDat_2016-06-08.Rds")

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
  geom_jitter(data = dayDat,aes(x=LUT,y=mean_temp),alpha=0)+
  geom_rect(data=shading,
            aes(xmin = min, xmax = max, ymin = -Inf, ymax = Inf,
                fill = factor(labPos)))+
  geom_jitter(data = dayDat,
              aes(x=LUT,y=mean_temp,
                  colour=forest_stratum,shape=season),
              alpha=0.4)+
  geom_boxplot(data= dayDat,aes(x=LUT,y=mean_temp),alpha=0.8,
               width=0.5)+
  geom_text(data=shading,
            aes(x = labPos,y = 8, label = labs),
            colour=rep(textCol,25),size=2.5)+
  theme_bw(base_size = 10)+
  ylab(expression(paste("Temperature (",degree*C,")",sep="")))+
  theme(axis.title.x = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_text(size=8,margin=margin(r = 5,unit = "pt")),
        axis.title.y = element_text(size=12,margin=margin(r = 5,unit = "pt")),
        axis.ticks.x = element_blank(),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
        strip.text=element_text(size=8),
        strip.background=element_rect(fill="white"),
        legend.position="top",
        legend.text=element_text(size=8),
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.spacing=unit(0.2,"cm"),
        legend.box = "horizontal",
        legend.box.just = "left",
        panel.grid=element_blank())+
  scale_y_continuous(breaks=seq(0,100,5))+
  scale_fill_manual(values = shades,guide=FALSE)+
  scale_colour_manual(values=cbPalette[4:3],
                      guide=guide_legend(order=1))+
  facet_wrap(~studyID)

# save(shading, p1, file = "../Thesis version/Figs/FigS1.Rdata")
# save(shading, p1, file = "../../Thesis/figs/figA1.1.Rdata")
# ggsave(filename = "../Ecology and Evolution/Revisions/Figures/ESM_1.png",
#        dpi=800,width=16.6,height=23,units = "cm",plot=p1)
ggsave(filename = "../Figures/Submission/FigS1.pdf",
       dpi=800,width=16.6,height=23,units = "cm",plot=p1)
