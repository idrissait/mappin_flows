library(plyr)
library(ggplot2)
library(maptools)
library(plyr)
library(dplyr)

input<-read.table("~/Downloads/201904-capitalbikeshare-tripdata.csv", sep=",", header=T)

centroids<- read.csv("~/Downloads/Capital_Bike_Share_Locations.csv")

or.xy<- merge(input, centroids, by.x="Start.station", by.y="ADDRESS")

destination.xy<-  merge(or.xy, centroids, by.x="End.station", by.y="ADDRESS")

destin.xy <- subset(destination.xy, select = c(LONGITUDE.x, LATITUDE.x, LONGITUDE.y, LATITUDE.y, Member.type, Duration))

names(destin.xy)<- c("oX", "oY", "dX", "dY", "type", "Duration")


destini.xy <- ddply(destin.xy,~oX + oY + dX + dY + type,summarise,Duration=sum(Duration))

#remove extremely far for readability in DC centre
summary(dest.xy)

dest.xy <- destini.xy %>%
  filter(oX > -77.1) %>%
  filter(oY < 39.25) %>%
  filter(dX < -76.91) %>%
  filter(dY > 38.81)

xquiet<- scale_x_continuous("", breaks=NULL)
yquiet<-scale_y_continuous("", breaks=NULL)
quiet<-list(xquiet, yquiet)

ggplot(dest.xy, aes(oX, oY))+
  #The next line tells ggplot that we wish to plot line segments. The "alpha=" is line transparency and used below 
  geom_segment(aes(x=oX, y=oY,xend=dX, yend=dY, alpha=Duration), col="white")+
  #Here is the magic bit that sets line transparency - essential to make the plot readable
  scale_alpha_continuous(range = c(0.01, 0.5))+
  #Set black background, ditch axes and fix aspect ratio
  theme(panel.background = element_rect(fill='black',colour='black'))+quiet+coord_equal()

