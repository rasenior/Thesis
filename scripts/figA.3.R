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
load(file.path(baseDir,"modelOutputs_dayMax_2016-10-15.Rd"))
load(file.path(baseDir,"sampleSizes_dayMax_2016-10-22.Rds"))
sumDat_day<-sumDat
sumLUT_day<-sumLUT
sumStats_dayMax<-sumStats

load(file.path(baseDir,"modelOutputs_nightMax_2016-10-15.Rd"))
load(file.path(baseDir,"sampleSizes_nightMax_2016-10-22.Rds"))
sumDat_night<-sumDat
sumLUT_night<-sumLUT
sumStats_nightMax<-sumStats

load(file.path(baseDir,"modelOutputs_dayMin_2016-10-15.Rd"))
load(file.path(baseDir,"sampleSizes_dayMin_2016-10-15.Rds"))
sumStats_dayMin<-sumStats

load(file.path(baseDir,"modelOutputs_nightMin_2016-10-15.Rd"))
load(file.path(baseDir,"sampleSizes_nightMin_2016-10-15.Rds"))
sumStats_nightMin<-sumStats

# load("modelOutputs_dayRange_2016-10-22.Rd")
# load("sampleSizes_dayRange_2016-10-22.Rds")
# sumStats_dayRange<-sumStats
# 
# load("modelOutputs_nightRange_2016-10-22.Rd")
# load("sampleSizes_nightRange_2016-10-22.Rds")
# sumStats_nightRange<-sumStats

load(file.path(baseDir,"modelOutputs_range_2016-10-23.Rd"))
load(file.path(baseDir,"sampleSizes_tempRange_2016-10-23.Rds"))
sumStats_range<-sumStats
sumLUT_range<-sumLUT

rm(newdat,sumDat,sumLUT,sumStats)

# Combine everything into one dataframe
tidyDF<-function(x,time){
  x<-x[,c(1,7,9)]
  
  info<-unlist(strsplit(deparse(substitute(x)),"_"))[2]
  
  thisMetric<-names(x)[3]
  
  x$time<-time
  x$metric<-thisMetric
  
  colnames(x)[which(names(x)==thisMetric)]<-"value"
  
  return(x)
  
}

sumStats_dayMax<-tidyDF(sumStats_dayMax,"Day")
sumStats_dayMin<-tidyDF(sumStats_dayMin,"Day")
# sumStats_dayRange<-tidyDF(sumStats_dayRange,"Day")

sumStats_nightMax<-tidyDF(sumStats_nightMax,"Night")
sumStats_nightMin<-tidyDF(sumStats_nightMin,"Night")
# sumStats_nightRange<-tidyDF(sumStats_nightRange,"Night")

sumStats_range<-tidyDF(sumStats_range,NA)

# Combine together
sumStats<-rbind(sumStats_dayMax,sumStats_dayMin,
                sumStats_nightMax,sumStats_nightMin)

sumStats$metric<-factor(sumStats$metric,
                        levels=unique(sumStats$metric),
                        labels=c("Max. temp.","Min. temp."))

sumLUT_day$time<-"Day"
sumLUT_night$time<-"Night"

sumLUT<-rbind(sumLUT_day,sumLUT_night)

sumLUT<-sumLUT[,c("LUT","N_sources","time")]
sumLUT_range<-sumLUT_range[,c("LUT","N_sources")]

sumStats<-merge(sumStats,sumLUT,by=c("LUT","time"))
sumStats_range<-merge(sumStats_range,sumLUT_range,by="LUT")

sumStats<-sumStats[sumStats$LUT!="Primary vegetation",]
sumStats_range<-sumStats_range[sumStats_range$LUT!="Primary vegetation",]
sumLUT<-sumLUT[sumLUT$LUT!="Primary vegetation",]

sumStats$LUT <- factor(sumStats$LUT,
                       levels = c("Degraded vegetation",
                                  "Plantation"),
                       labels=c("Degraded forest",
                                "Plantation"))

sumStats_range$LUT <- factor(sumStats_range$LUT,
                       levels = c("Degraded vegetation",
                                  "Plantation"),
                       labels=c("Degraded forest",
                                "Plantation"))



# Add significance level for LUT
sigResults<-data.frame(result=c(0.0876,0.1007,0.04183,0.315),
                       LUT="Plantation",value=4,
                       time=c(rep("Day",2),rep("Night",2)),
                       metric=rep(c("Max. temp.","Min. temp."),2))

#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
sigResults$sigLevel<-
  ifelse(sigResults$result > 0.05,"n.s.",
         ifelse(sigResults$result <= 0.05 & sigResults$result > 0.01,"*",
                ifelse(sigResults$result <= 0.01 & sigResults$result > 0.001,"**",
                       ifelse(sigResults$result <= 0.001,"***",NA))))

sigResults$label<-paste("(",letters[1:4], ")", sep="")

sigResults_range<-data.frame(result=0.06926,LUT="Plantation",
                             value=4,metric="Temp. range",sigLevel="n.s.",
                             label="(e)")


p1<-ggplot(sumStats, aes(x=LUT, y=value))+
  geom_point(position = position_dodge(width = 0.4),size=2)+
  geom_hline(yintercept =0,linetype="dashed",colour="black")+
  geom_point(data=sumStats,aes(x=LUT, y=value),
             position = position_dodge(width = 0.4),size=2)+
  geom_errorbar(aes(ymin = value-CI, 
                    ymax = value+CI),width=0.1,
                position = position_dodge(width = 0.4))+
  geom_text(aes(x=rep(c(1.1,2.1),4),y=1,label=N_sources),
            size=2,colour="grey10")+
  theme_bw()+
  facet_grid(time~metric)+
  ylab(expression(paste("Temperature difference (",degree*C,")",sep="")))+
  geom_rect(data=sigResults,aes(xmin=2.4,xmax=2.595,ymin=6,ymax=8.8),
            fill="#CCCCCC",colour="#959595")+
  theme(axis.title.x = element_blank(),
        axis.text.x=element_text(size= title_size,
                                 margin=margin(t = 5, unit = "pt"),
                                 colour = "black"),
        axis.text.y=element_text(size= text_size,
                                 margin=margin(r = 5, unit = "pt")),
        axis.title.y = element_text(size= title_size), 
        strip.text = element_text(size= text_size),
        axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
        axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'),
        legend.position=c(0.23,0.85),
        legend.text.align	= 0,
        legend.title=element_blank(),
        legend.key=element_blank(),
        legend.box = "horizontal",
        legend.box.just = "left",
        panel.grid=element_blank(),
        panel.border = element_blank() ,
        # panel.margin=unit(c(0,0.5,0.5,0.5), "lines"),
        panel.background = element_blank())+
  scale_y_continuous(breaks=seq(-100,100,2),
                     limits=c(-8.5,8.84),
                     expand = c(0,0))+
  scale_shape_discrete(guide=guide_legend(order=2))+
  scale_fill_manual(values=cbPalette[c(4,7)],
                    guide=guide_legend(order=1))+
  scale_colour_manual(values=cbPalette[c(4,7)],
                      guide=guide_legend(order=1))

combi <-
    ggdraw() +
    draw_plot(p1, x = 0, y = 0, width = 1, height = 1) +
    draw_plot_label(label = c("n.s.", "n.s.", "*", "n.s."),
                    x = rep(c(0.27, 0.71),2),
                    y = rep(c(0.85, 0.42), each = 2),
                    size = c(8,8,11,8)) +
    draw_plot_label(label = c("(a)", "(b)", "(c)", "(d)"),
                    x = rep(c(0.474, 0.914),2),
                    y = rep(c(0.905, 0.475), each = 2),
                    size = lab_size)
combi
saveRDS(combi, file = "figs/figA.5.Rds")


