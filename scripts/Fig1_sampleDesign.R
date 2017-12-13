
# Setup -------------------------------------------------------------------

rm(list=ls())

# Get the current user (Rebecca or Becc, if work or home)
user<-Sys.info()[7]

# Set working directory
outDir<-file.path("C:/Users",user,"Google Drive/PhD/Thesis/figs")
map_dir<-file.path("C:/Users",user,"Google Drive/PhD/Ch4/Methods/Sites")

# Get current data
date<-Sys.Date()

# Load libraries
library(rgdal)
library(rgeos)
library(raster)
library(maptools)
library(ggplot2)
library(gridExtra)
library(grid)
library(cowplot)
library(RColorBrewer)

source(file.path("C:/Users",user,"Google Drive/R/functions/geom_holygon.R"))
source(file.path("C:/Users",user,"Google Drive/R/functions/map_scalebar.R"))

draw_circle <- function(center = c(0,0),diameter = 1, npoints = 100){
  r = diameter / 2
  tt <- seq(0,2*pi,length.out = npoints)
  xx <- center[1] + r * cos(tt)
  yy <- center[2] + r * sin(tt)
  return(data.frame(x = xx, y = yy))
}

# Define colourblind-friendly palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
               "#0072B2", "#D55E00", "#CC79A7")
bwPalette<-c("black","grey50",brewer.pal(4,"Greys"))

borneo<-readOGR(file.path(map_dir,"Borneo countries"),"borneo") 
borneo_crop<-crop(borneo,extent(113,127,3.5,7))
sabah<-readOGR(file.path(map_dir,"Main maps"),"sabah_outline")
danum<-readOGR(file.path(map_dir,"Main maps"),"danum_area") 
danum_crop<-crop(danum,extent(117.59,117.98,5.12,4.9))
usmfr<-readOGR(file.path(map_dir,"Main maps"),"logging_concession")
usmfr_crop<-crop(usmfr,extent(117.59,117.98,5.12,4.9))
pol<-data.frame(xmin=117.59,xmax=117.98 ,ymin=5.12 ,ymax=4.9)

plotInfo<-
  readRDS(file.path("C:/Users",user,
                    "Google Drive/PhD/Ch4/Resubmission/Data/FINAL/plot_info_2017-07-27.Rds"))
plotInfo<-plotInfo[!duplicated(plotInfo$transect),]

plotInfo$forest_type<-factor(plotInfo$forest_type,
                            levels=c("Primary","Logged"))


# Sabah map ---------------------------------------------------------------

p1<-ggplot()+
  geom_polygon(data=borneo_crop,aes(x=long, y=lat, group=group),
               fill="grey80")+
  geom_polygon(data=sabah,aes(x=long, y=lat, group=group),
               fill="grey60")+
  geom_holygon(data=usmfr,aes(x=long, y=lat, group=group),
               fill=cbPalette[7],alpha=0.7)+
  geom_polygon(data=danum,aes(x=long, y=lat, group=group),
               fill=cbPalette[6],alpha=0.7)+
  geom_rect(data = pol, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            alpha=0, colour="black", size = 0.5, linetype=1)+
  geom_segment(aes(x=115,xend=115,y=6.5,yend=7),
               size=0.5,arrow = arrow(length = unit(0.15,"cm"),
                                      ends = "last",type = "closed"))+
  geom_text(aes(x=115,y=6.3,label="N"),size=3)+
  theme_classic()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        # plot.background = element_rect(fill="pink"),
        plot.margin = margin(0.1,0.1,0.1,0.1,unit = "cm"))+
  scale_x_continuous(breaks=NULL,expand = c(0, 0))+
  scale_y_continuous(breaks=NULL,expand = c(0, 0))+
  coord_equal(ratio=1)

# Danum map ---------------------------------------------------------------

p2<-ggplot()+
  geom_rect(data = pol, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill="grey60", size = 0.5)+
  geom_holygon(data=usmfr_crop,aes(x=long, y=lat, group=group),
               fill=cbPalette[7],alpha=0.7)+
  geom_polygon(data=danum_crop,aes(x=long, y=lat, group=group),
               fill=cbPalette[6],alpha=0.7)+
  # geom_point(data=plotInfo, aes(x=longitude, y=latitude,fill=forest_type),
  #            size=3,shape=22,colour="black")+
  geom_rect(data=plotInfo, 
            aes(xmin=longitude-0.003,xmax=longitude+0.003,
                ymin=latitude-0.006,ymax=latitude+0.006,
                fill=forest_type),colour="black")+
  geom_rect(data = pol, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            alpha=0,colour="black",size = 2, linetype=1)+
  theme_classic()+
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        # plot.background=element_rect(fill="pink"),
        plot.margin = margin(0.1,0.1,0,0.1,unit = "cm"))+
  scale_x_continuous(breaks=NULL,expand = c(0, 0))+
  scale_y_continuous(breaks=NULL,expand = c(0, 0))+
  scale_fill_manual(values=c(cbPalette[6],cbPalette[7]),
                      labels=c("Primary","Logged"))+
  coord_equal(ratio=1)+
  guides(colour=FALSE,fill=FALSE)+
  scale_bar(lon = 117.83, lat = 5.05, 
              distance_lon = 5, distance_lat = 0.5, distance_legend = 1.8, 
              dist_unit = "km", orientation = FALSE)


# Transect layout ---------------------------------------------------------

schematic<-data.frame(x=rep(1,10),
                      y=rep(seq(0,550,125),2),
                      blob=rep(c("small","big"),each=5),
                      plotID=rep(paste("P",1:5,sep=""),2))

p3<-ggplot(schematic,aes(x=x,y=y,size=blob,fill=blob))+
  geom_point(shape=21,colour="black")+
  geom_rect(aes(xmin=0.8,xmax=1.2,ymin=-70,ymax=570),
            fill=NA,colour="black",size=0.5)+
  geom_segment(aes(x=1.3,xend=1.3,y=250,yend=375),
               size=0.2,arrow = arrow(length = unit(0.2,"cm"),
                                      ends = "both",type = "open"))+
  # geom_text(aes(x = 1.43,y=mean(c(250,375))),
  #           label="125 m",size=2)+
  geom_text(aes(y=y+25,label = plotID),size=2)+
  theme_void()+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        plot.margin=margin(0,0,0,0))+
  scale_fill_manual(values=c("black","white"))+
  scale_size_manual(values=c(0.5,8))+
  scale_y_continuous(limits=c(-71,571))+
  scale_x_continuous(limits=c(0,2))+
  guides(size=FALSE,fill=FALSE)

# Plot layout -------------------------------------------------------------

circle <- draw_circle(center = c(0,0),diameter = 20,npoints = 100)

# Define segment 1
seg1<-draw_circle(center = c(0,0),diameter = 17,npoints = 100)
seg1<-seg1[1:25,]
seg1$x[25]<-0
seg1<-rbind(seg1,cbind(x=0,y=0))

# Define segment 2
seg2<-draw_circle(center = c(0,0),diameter = 10,npoints = 100)
seg2<-seg2[26:50,]
seg2$x[1]<-0
seg2$y[25]<-0
seg2<-rbind(seg2,cbind(x=0,y=0))

# Define segment 3
seg3<-draw_circle(center = c(0,0),diameter = 14,npoints = 100)
seg3<-seg3[51:75,]
seg3$y[1]<-0
seg3$x[25]<-0
seg3<-rbind(seg3,cbind(x=0,y=0))

# Define segment 4
seg4<-draw_circle(center = c(0,0),diameter = 13,npoints = 100)
seg4<-seg4[76:100,]
seg4$x[1]<-0
seg4<-rbind(seg4,cbind(x=0,y=0))

# Define points to symbolise furthest trees
pts<-rbind(seg1[8,],cbind(x=1,y=4),
           seg2[20,],cbind(x=-3,y=2),
           seg3[3,],cbind(x=-2,y=-1),
           seg4[15,],cbind(x=4,y=-4),
           cbind(x=0,y=0))

pts$distance<-c(rep(c("Furthest","Nearest"),4),"Plot centre")

lineGrad<-pts[1,2]/pts[1,1]

p4<-ggplot()+
  geom_polygon(data = circle,aes(x,y),fill=cbPalette[1],
               alpha=0,colour="black")+
  geom_polygon(data = seg1,aes(x,y),fill=cbPalette[3])+
  geom_polygon(data = seg2,aes(x,y),fill=cbPalette[4])+
  geom_polygon(data = seg3,aes(x,y),fill=cbPalette[5])+
  geom_polygon(data = seg4,aes(x,y),fill=cbPalette[8])+
  geom_point(data = pts, aes(x,y,shape=distance),size=1.5)+
  geom_segment(aes(x = 0.5 , y = 0.5*lineGrad , 
                   xend = pts[1,1] - 0.5, 
                   yend = (pts[1,1] - 0.5)*lineGrad),
               arrow = arrow(length = unit(0.1, "cm"),ends = "both"))+
  geom_text(aes(x=pts[1,1]/2,y=(pts[1,2]/2)+0.8,label="r"),size=2.5)+
  geom_segment(aes(x=min(circle$x),xend=max(circle$x),
                   y=0,yend=0),linetype="dashed")+
  geom_segment(aes(y=min(circle$y),yend=max(circle$y),
                   x=0,xend=0),linetype="dashed")+
  geom_text(aes(x=c(6.5,-6.5,-6.5,6.5),
                y=c(6.5,6.5,-6.5,-6.5),
                label=c("Q2","Q1","Q3","Q4")),
            size=2)+
  theme_void()+
  scale_shape_manual(values=c(8,4,19))+
  guides(shape=FALSE)


# Arrow guides ------------------------------------------------------------

arrow1<-ggplot()+
  geom_curve(aes(x = 1, xend = 2, y = 1.5, yend = 1.5),
           curvature = -0.1, size=0.8,
           arrow = arrow(length = unit(0.15,"cm"),
                         ends = "last"))+
  scale_x_continuous(breaks = c(1,1.5,2),limits = c(1,2))+
  scale_y_continuous(breaks = c(1,1.5,2),limits = c(1,2))+
  theme_void()

arrow2<-ggplot()+
  geom_curve(aes(y = 2, yend = 1, x = 1.5, xend = 1.5),
             curvature = -0.1, size=0.8,
             arrow = arrow(length = unit(0.15,"cm"),
                           ends = "last"))+
  scale_x_continuous(breaks = c(1,1.5,2),limits = c(1,2))+
  scale_y_continuous(breaks = c(1,1.5,2),limits = c(1,2))+
  theme_void()

arrow3<-ggplot()+
  geom_curve(aes(x = 2, xend = 1, y = 1.5, yend = 1.5),
             curvature = -0.1, size=0.8,
             arrow = arrow(length = unit(0.15,"cm"),
                           ends = "last"))+
  scale_x_continuous(breaks = c(1,1.5,2),limits = c(1,2))+
  scale_y_continuous(breaks = c(1,1.5,2),limits = c(1,2))+
  theme_void()

# Combine and save --------------------------------------------------------

combi <-
  ggdraw()+
  draw_plot(p1, x = 0, y= 0.5,width=0.5,height=0.5)+
  draw_plot(p4, x = 0.112, y= 0,width=0.27,height=0.5)+
  draw_plot(p2, x = 0.47, y= 0.5,width=0.5,height=0.5)+
  draw_plot(p3, x = 0.47, y= 0,width=0.5,height=0.5)+
  draw_plot(arrow1, x = 0.434, y= 0.6,width=0.1,height=0.5)+
  draw_plot(arrow2, x = 0.68, y= 0.405,width=0.5,height=0.18)+
  draw_plot(arrow3, x = 0.434, y= -0.09,width=0.1,height=0.5)+
  draw_plot_label(label = c("(a)", "(b)", "(d)","(c)"),
                  x = c(0, 0.5,0, 0.5),
                  y = c(0.98, 0.98, 0.48,0.48),
                  size = 9,
                  hjust = c(-0.5,0.3,-0.5,0.3),
                  vjust = c(1.7,1.7,1.4,1.4))+
    draw_plot_label(label = "125 m",
                    x = 0.78, 
                    y = 0.32,
                    size = 7)

ggsave(plot = combi,
       filename = file.path(outDir,"fig4.1.png"),
       dpi= 400,width = 16.6,height=9,units="cm")

saveRDS(combi,file = file.path(outDir,"fig4.1.Rds"))
