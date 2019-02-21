data=read.csv("~/Desktop/RStudio/Data141a/the_hawks.csv")

library(ggplot2)
library(ggmap)
library(spatial)
library(sp)
library(sf)
library(maps)
library(dplyr)
library(lubridate)
library(geosphere)
library(ggrepel)

names(data)[names(data)=="long"]<-"lon"
loc1.1 = sapply(data[c("lon","lat")], function(x)median(x,na.rm=TRUE)) 
m=get_map(loc1.1,zoom=10)
ggmap(m)+geom_point(aes(lon,lat, color=factor(tag)),data, alpha=.5, na.rm=TRUE)+
  xlab("Longitude")+ylab("Latiude")+ggtitle("Hawk Map for each Tag")+scale_color_discrete(name="tag")

#Function created to find the center of the map using median
#Get_map used to create the map of the location using known cordinates
#Map plotted

gg=ggmap(m)+geom_point(aes(lon,lat, color=factor(tag)),data, alpha=.5, na.rm=TRUE)+
  xlab("Longitude")+ylab("Latiude")+ggtitle("Hawk Map")

subsetdata = subset(data,stage == "arrival" )

ggmap(m)+geom_line(aes(lon,lat, color=factor(tag), alpha=.5, na.rm=TRUE),subsetdata)+
  xlab("Longitude")+ylab("Latiude")+ggtitle("Hawk Map")

subsetdata28 = subset(subsetdata, tag==105928)
loc1.2 = sapply(subsetdata28[c("lon","lat")], function(x)median(x,na.rm=TRUE))
n=get_map(loc1.2,zoom=15)
ggmap(n, base_layer = ggplot( subsetdata28, aes(lon,lat)))+
  geom_point(aes(color=speed,size=height),alpha=.5, na.rm = TRUE)+
  xlab("Longitude")+ylab("Latiude")+ggtitle("Hawk Map on Height and Speed for 105928")+
  geom_line(aes(lon,lat),subsetdata28)+
  geom_segment(aes(xend = c(tail(lon, n=-1), NA), yend=c(tail(lat, n=-1),NA) ),
               arrow = arrow(length = unit(0.3, "cm") ) )+
  geom_label_repel(data=tail(subsetdata28,1),aes(label='end'),
                   color='red',point.padding = .5,segment.color = 'red')+
  geom_label_repel(data=tail(subsetdata28,1),aes(label='start'),
                   color='red',point.padding = .5,segment.color = 'red')

subsetdata36 = subset(subsetdata,tag==105936)
loc1.3 = sapply(subsetdata36[c("lon","lat")], function(x)median(x,na.rm=TRUE))
o=get_map(loc1.3,zoom=15)
ggmap(o, base_layer = ggplot( subsetdata36, aes(lon,lat)))+
  geom_point(aes(color=speed,size=height),alpha=.5, na.rm = TRUE)+
  xlab("Longitude")+ylab("Latiude")+ggtitle("Hawk Map on Height and Speed for 105936")+
  geom_segment(aes(xend = c(tail(lon, n=-1), NA), yend=c(tail(lat, n=-1),NA) ),
               arrow = arrow(length = unit(0.3, "cm") ) )+
  geom_label_repel(data=tail(subsetdata36,1),aes(label='end'),
                   color='red',point.padding = .5,segment.color = 'red')+
  geom_label_repel(data=tail(subsetdata36,1),aes(label='start'),
                   color='red',point.padding = .5,segment.color = 'red')

#Data subset to only include "arrival" stage
#Functions similar to the first plot is done once again
#Includes the direction using the geom_segment and the start and end location using geom_label_repel

subsetdata3=subset(data, speed == 0)
loc1.4 = sapply(subsetdata3[c("lon","lat")], function(x)median(x,na.rm=TRUE))
p=get_map(loc1.4,zoom=10)
ggmap(p)+geom_point(aes(lon,lat, color=speed,size=height),subsetdata3, alpha=.5, na.rm=TRUE)+
  xlab("Longitude")+ylab("Latiude")+ggtitle("Hawk Map")

subsetdata3.1=subset(data, data$tag == "105923")
subsetdata3.2=subset(data, data$tag == "105928")
subsetdata3.3=subset(data, data$tag == "105930")
subsetdata3.4=subset(data, data$tag == "105936")
subsetdata3.5=subset(data, data$tag == "117527")

loc3.1 = sapply(subsetdata3.1[c("lon","lat")], function(x)median(x,na.rm=TRUE))
loc3.2 = sapply(subsetdata3.2[c("lon","lat")], function(x)median(x,na.rm=TRUE))
loc3.3 = sapply(subsetdata3.3[c("lon","lat")], function(x)median(x,na.rm=TRUE))
loc3.4 = sapply(subsetdata3.4[c("lon","lat")], function(x)median(x,na.rm=TRUE))
loc3.5 = sapply(subsetdata3.5[c("lon","lat")], function(x)median(x,na.rm=TRUE))

subsetdata3.1 = subsetdata3.1 %>% 
  mutate(dist = by(subsetdata3.1, 1:nrow(subsetdata3.1), function(row){distGeo(c(row$lon,row$lat),loc3.1)}))
subsetdata3.2 = subsetdata3.2 %>% 
  mutate(dist = by(subsetdata3.2, 1:nrow(subsetdata3.2), function(row){distGeo(c(row$lon,row$lat),loc3.2)}))
subsetdata3.3 = subsetdata3.3 %>% 
  mutate(dist = by(subsetdata3.3, 1:nrow(subsetdata3.3), function(row){distGeo(c(row$lon,row$lat),loc3.3)}))
subsetdata3.4 = subsetdata3.4 %>% 
  mutate(dist = by(subsetdata3.4, 1:nrow(subsetdata3.4), function(row){distGeo(c(row$lon,row$lat),loc3.4)}))
subsetdata3.5 = subsetdata3.5 %>% 
  mutate(dist = by(subsetdata3.5, 1:nrow(subsetdata3.5), function(row){distGeo(c(row$lon,row$lat),loc3.5)}))

subsetdata3.1$dist=as.numeric(subsetdata3.1$dist)
subsetdata3.2$dist=as.numeric(subsetdata3.2$dist)
subsetdata3.3$dist=as.numeric(subsetdata3.3$dist)
subsetdata3.4$dist=as.numeric(subsetdata3.4$dist)
subsetdata3.5$dist=as.numeric(subsetdata3.5$dist)

#Using distGeo function to calculate the distance between points
#However, the data is transfromed into a different data class and we change it back to a numeric data

ggplot(subsetdata3.1, aes(x=1:nrow(subsetdata3.1),y=subsetdata3.1$dist/1609) )+
  geom_point(color="blue")+ggtitle("Hawk Map for 105923")+xlab("Time")+ylab("Distance")+geom_line()
ggplot(subsetdata3.2, aes(x=1:nrow(subsetdata3.2),y=subsetdata3.2$dist/1609) )+
  geom_point(color="red")+ggtitle("Hawk Map for 105928")+xlab("Time")+ylab("Distance")+geom_line()
ggplot(subsetdata3.3, aes(x=1:nrow(subsetdata3.3),y=subsetdata3.3$dist/1609) )+
  geom_point(color="blue")+ggtitle("Hawk Map for 105930")+xlab("Time")+ylab("Distance")+geom_line()
ggplot(subsetdata3.4, aes(x=1:nrow(subsetdata3.4),y=subsetdata3.4$dist/1609) )+
  geom_point(color="red")+ggtitle("Hawk Map for 105936")+xlab("Time")+ylab("Distance")+geom_line()
ggplot(subsetdata3.5, aes(x=1:nrow(subsetdata3.5),y=subsetdata3.5$dist/1609) )+
  geom_point(color="blue")+ggtitle("Hawk Map for 117527")+xlab("Time")+ylab("Distance")+geom_line()
##28,36
#Plot the distance vs time

as.POSIXlt(data$time)
data$time=as.Date(data$time)

#Convert time into a date variable

subsetdata3.2$time=parse_date_time(subsetdata3.2$time,"y-m-d H:M:S")
date1= parse_date_time("2012-09-12 23:00:00", "y-m-d H:M:S")
subsetdata3.2.1=subset(subsetdata3.2, subsetdata3.2$time >= date1)
#2012-09-12 23:00:00
#2012-09-14 03:00:00
subsetdata3.4$time=parse_date_time(subsetdata3.4$time,"y-m-d H:M:S")
date2= parse_date_time("2012-08-05 02:00:00", "y-m-d H:M:S")
subsetdata3.4.1=subset(subsetdata3.4, subsetdata3.4$time >= date2)
#2012-08-05 02:00:00
#2012-08-06 02:00:00
subsetdata3.2.1$times=as.factor(subsetdata3.2.1$time)
subsetdata3.4.1$times=as.factor(subsetdata3.4.1$time)

#Subset the data for the final days while leaving its nest

ggplot(subsetdata3.2, aes(x=(subsetdata3.2$time),y=subsetdata3.2$dist/1609) )+
  geom_point(color="red")+ggtitle("Hawk Map for 105928")+
  xlab("Time (months)")+ylab("Distance (miles)")+geom_line()
ggplot(subsetdata3.4, aes(x=(subsetdata3.4$time),y=subsetdata3.4$dist/1609) )+
  geom_point(color="red")+ggtitle("Hawk Map for 105936")+
  xlab("Time (months)")+ylab("Distance (miles)")+geom_line()

#Plot the graphs for the hawks that left their nests

loc4.1 = sapply(subsetdata3.2.1[c("lon","lat")], function(x)median(x,na.rm=TRUE)) 
t=get_map(loc4.1,zoom=10)
ggmap(t, base_layer = ggplot(subsetdata3.2.1, aes(lon, lat)))+
  geom_point(aes(color=speed,size=height), alpha=.5, na.rm=TRUE)+
  xlab("Longitude")+ylab("Latiude")+ggtitle("Hawk Map for 105928")+
  geom_label_repel(aes(label=times), box.padding=3, segment.size = .2 )+
  geom_segment(aes(xend = c(tail(lon, n=-1), NA), yend=c(tail(lat, n=-1),NA) ),
               arrow = arrow(length = unit(0.3, "cm") ) )

loc4.2 = sapply(subsetdata3.4.1[c("lon","lat")], function(x)median(x,na.rm=TRUE)) 
u=get_map(loc4.1,zoom=10)
ggmap(u, base_layer = ggplot(subsetdata3.4.1, aes(lon, lat)))+
  geom_point(aes(color=speed,size=height), alpha=.5, na.rm=TRUE)+
  xlab("Longitude")+ylab("Latiude")+ggtitle("Hawk Map for 105936")+
  geom_label_repel(aes(label=times), box.padding=3, segment.size = .2 )+
  geom_segment(aes(xend = c(tail(lon, n=-1), NA), yend=c(tail(lat, n=-1),NA) ),
               arrow = arrow(length = unit(0.3, "cm") ) ) 

#Plot the final days the hawk leaves its nest similar to before
#Using geom_label_repel to label the points with their date and time
#Using geom_segments to connect the points together

#https://cran.r-project.org/web/packages/ggrepel/vignettes/ggrepel.html
#Collabrated with Huong Vu