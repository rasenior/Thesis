
# Setup -------------------------------------------------------------------

# Load libraries
library(ggplot2)
library(gridExtra)
library(grid)
library(cowplot)
library(RColorBrewer)

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

p1<-ggplot()+
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

# Save this separately for SOM
saveRDS(p1,file = "figs/figB.1.Rds")

