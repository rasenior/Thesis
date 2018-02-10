rm(list=ls())

user<-Sys.info()[7]

#Define working directory
baseDir<-file.path("C:/Users",user,"Google Drive/PhD/Ch2/Data")

# Read in data
sources<-read.csv(file.path(baseDir,"sourceInfo_day_2016-06-08.csv"))

# Define colourblind-friendly palette
cbPalette_light <- c("#009E73","#F0E442", "#0072B2", "#D55E00", "#CC79A7",
                     "#E69F00", "#56B4E9")
cbPalette_dark<-c("#66A61E","#1B9E77","#7570B3","#D95F02","#E6AB02")
text_size <- 8
title_size <- 10

sizeList<-seq(5.5,0.5,length.out = 5)

# Load libraries
library(rgdal)
library(maptools)
library(ggplot2)
library(cowplot)
library(ggrepel)

dim(sources)
# [1] 25  19

g_legend<-function(a.gplot){
      tmp <- ggplot_gtable(ggplot_build(a.gplot))
      leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
      legend <- tmp$grobs[[leg]]
      return(legend)}

mapDir<-"C:/Users/Rebecca/Google Drive/Spatial-data/Country outlines"
tropics<-readOGR(file.path(mapDir, "Tropics"),"tropics") 
tropicsExtent<-readOGR(file.path(mapDir, "Tropics"),"tropics_extent") 
tropicsCountries<-readOGR(file.path(mapDir, "Tropics"),"tropics_byCountry") 
world<-readOGR(file.path(mapDir,"World"),"world_wo_Antarctica") 

# Want to display combinations of LUT
# 1 = PF + DF
# 2 = PF + DF + Pl
# 3 = DF + Pl
# 4 = PF + Pa
# 5 = DF + Pa
# 6 = PF + Pa + Cr
# 7 = DF + Cr


sources$combo<-
      ifelse(sources$primary==TRUE & sources$degraded==TRUE & sources$plantation==FALSE,1,
             ifelse(sources$primary==TRUE & sources$degraded==TRUE & sources$plantation==TRUE,2,
                    ifelse(sources$primary==FALSE & sources$degraded==TRUE & sources$plantation==TRUE,3,
                           ifelse(sources$primary==TRUE & sources$pasture==TRUE & sources$cropland==FALSE,4,
                                  ifelse(sources$degraded==TRUE & sources$pasture==TRUE,5,
                                         ifelse(sources$primary==TRUE & sources$pasture==TRUE & sources$cropland==TRUE,6,
                                                ifelse(sources$degraded==TRUE & sources$cropland==TRUE,7,NA)))))))

sources$labLat<-sources$latitude_site
sources$labLon<-sources$longitude_site

# Manually adjust each label's coordinates
# sources$labLat[1]<-sources$labLat[1]-7


# Source geom_holygo function
source(file.path("C:/Users",user,"Google Drive/Programming/R/functions/geom_holygon.R"))

p1<-ggplot() +
      geom_holygon(data=world, aes(x=long, y=lat, group=group),
                   fill="grey80")+
      geom_holygon(data=tropics, aes(x=long, y=lat, group=group),
                   fill="grey50",alpha=0.5)+
      geom_point(data=sources[sources$primary==TRUE,], 
                 aes(x=longitude_site, y=latitude_site),
                 colour=cbPalette_dark[1],size=sizeList[1],
                 alpha=0.6,shape=16)+ 
      geom_point(data=sources[sources$degraded==TRUE,], 
                 aes(x=longitude_site, y=latitude_site),
                 colour=cbPalette_dark[2],size=sizeList[2],
                 alpha=0.6,shape=16)+ 
      geom_point(data=sources[sources$plantation==TRUE,], 
                 aes(x=longitude_site, y=latitude_site),
                 colour=cbPalette_dark[3],size=sizeList[3],
                 alpha=0.6,shape=16)+ 
      geom_point(data=sources[sources$cropland==TRUE,], 
                 aes(x=longitude_site, y=latitude_site),
                 colour=cbPalette_dark[4],size=sizeList[4],
                 alpha=0.6,shape=16)+ 
      geom_point(data=sources[sources$pasture==TRUE,], 
                 aes(x=longitude_site, y=latitude_site),
                 colour=cbPalette_dark[5],size=sizeList[5],
                 alpha=0.6,shape=16)+ 
      geom_text_repel(data=sources,
                aes(x=labLon, y=labLat,label=sources),
                size=2,force=1.5,segment.size=0.1,
                min.segment.length = unit(0.1, "lines"),
                point.padding = unit(0.5, "lines"))+
      theme_classic()+
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.background=element_rect(fill="white"),
            plot.margin = (unit(c(0,0,0,0), "cm")))+
      scale_x_continuous(breaks=NULL,expand = c(0, 0))+
      scale_y_continuous(breaks=NULL,expand = c(0, 0))+
      coord_equal(ratio=1)

# Dummy
df<-data.frame(LUT=c("Primary forest","Degraded forest",
                     "Plantation","Pasture","Cropland"))
df$LUT<-factor(df$LUT,
               levels=c("Primary forest","Degraded forest",
                        "Plantation","Pasture","Cropland"))

dummy<-ggplot(df,aes(x=LUT,y=2,colour=LUT,size=LUT))+
      geom_point()+
      theme(legend.text=element_text(size= text_size),
            legend.key.size=unit(text_size,"pt"),
            legend.position="bottom",
            legend.key=element_blank(),
            legend.title=element_blank())+
      scale_size_manual(values=sizeList)+
      scale_colour_manual(values=cbPalette_dark[1:5])
# dummy

# extract the legend from one of the plots
# (clearly the whole thing only makes sense if all plots
# have the same legend, so we can arbitrarily pick one.)
#Extract Legend
legend <- g_legend(dummy)

# add the legend underneath the row we made earlier. Give it 10% of the height
# of one plot (via rel_heights).
p2<-plot_grid(p1,legend, ncol = 1, rel_heights = c(1,0.08))
p2

# saveRDS(p2,"../Thesis version/Figs/Fig1.Rds")
# saveRDS(p2,file = "figs/fig2.1.Rds")
save(p1, legend, file = "figs/fig2.1.Rdata")
