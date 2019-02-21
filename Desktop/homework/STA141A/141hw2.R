data=readRDS("~/Desktop/RStudio/Data141a/housing.rds")

library(tidyverse)
library(lubridate)
library(reshape2)
library(MASS)
library(lattice)
library(treemap)
library(plotly)
library(maps)
library(plyr)

#1
data$county <- as.character(data$county)
data$city <- as.character(data$city)
data$zip <- as.numeric(data$zip)
data$date<-as.Date(data$date)
data[-c(217299,170387,253208,329352,55530,332042,
        297936,207985,15580,18188,282465,48988,254395)]


data$year[which(data$year == 20005)] = 2005
data$year[which(data$year == 3885)] = 1885

data$county = gsub("Franciscoe", "Francisco", data$county)
data$county = gsub("county", "County", data$county)
data$county = gsub("Alpine","San Francisco", data$county)
data$city = gsub("Belvedere/tiburon", "Belvedere/Tiburon", data$city)

#2
range(data$year, na.rm = TRUE)
which.max(data$year)
which.min(data$year)

#1885, 2005
range(data$date, na.rm = TRUE)
#using the range function, we can find the difference between the start and end years
#By using which, we can find the which datapoint and correspond the date

##3
data.time=data[order(data$date),]
data$my = format.Date(data$date, "%Y-%m")
#as.Date(data$my)
data$year = strptime(data$year, "%Y")
data$year = ymd(data$year)
#Subset the date into Year and Month only then reformate into a Date variable
#Similarly, change year into a Date variable, we need to insert a month and day to fit
#the Y-M-D form of Date variable.

table1 = table(data$my)
mean2 = aggregate(price~my, data, mean, na.rm = TRUE)
#add "01" to convert character into Date
mean2$my= paste(mean2$my, "-01")
# converting 
date_agg= as.Date(mean2$my,"%Y-%m -%d")

plot(x=parse_date_time(names(table1),"y-m"),y=table1, type = 'b',
     xlab = "Date of Sales", ylab = "Number of Sales", main = "Sales per Month") 
# number of sales

plot(date_agg,mean2$price,type="l", main = "Average Price per year",
     xlab="Date of sales", ylab="Average Price")

#Afterwards plot the two graphs

data$year= format.Date(data$year, "%Y")

# Office Hours

#4

data$county
data$br
data$year

data$year = as.Date(data$year)

subsetdata = subset(data,format(as.Date(data$date),"%Y") >=2003 & 
                      format(as.Date(data$date),"%Y") <= 2005)
#Subset the data into a year range between 2003 and 2005
subsetdata$saleyear=year(subsetdata$date)
#Subset dates into years only
subsetdata$br=as.numeric(subsetdata$br)
subsetdata$brcut=cut(subsetdata$br, c(0,1,2,3,Inf), c("1","2","3","4+"))
#Use cut function to distenguish between 4 numbers
data_agg=aggregate(price~county+saleyear+brcut,mean,data=subsetdata, na.rm=TRUE)
ggplot(data_agg, aes(x=saleyear, y=price, col=brcut))+geom_point()+
  geom_line()+facet_wrap(~county)+scale_x_continuous(breaks = c(2003,2004,2005))+
  ggtitle(" Sales per year by Price")
#Plot the graph

#Office hours

#5
city=data$city

citysales=lapply(city, function(x,y) 
  y = length(table(data$county[which(data$city == x)])))
#Use lapply to loop a function with which to find the counts
citysales2 = which(citysales == 2)
citysales3 = lapply(citysales2, function(x,y) y = city[x])
table(data$county[which(data$city == citysales3[1])])
table(data$county[which(data$city == citysales3[2])])

which(rowSums(table(unique(data[c("city","county")])))>1)
#Find which city sales appear more than once

#Collaborated with Linlin Liu

#6
xx=data[-c(16489, 6314, 827,16502, 6322, 829, 16499, 6320, 828),]
bsqft=xx$bsqft
price=xx$price

z=cbind(bsqft,price)
z=na.omit(z)

colnames(z)<-c("bsqft","price")

plot(z[,1],z[,2], xlab="building (FT)", ylab="price")
boxcox(z[,1]~z[,2])
qqplot(z[,1],z[,2], xlab="building (FT)", ylab="price",main = "QQPlot")
yyy=lm(z[,2]~z[,1])
summary(yyy)
abline(yyy)
par(mfrow=c(2,2))
plot(yyy, pch=20,cex=.5)

bsqft2=log(z[,1])
price2=log(z[,2]+1)
price3=ifelse(data$price <=0, NA, data$price)
price3=log(price3)

t=cbind(bsqft2,price2)

plot(bsqft2,price2, xlab="building (FT)", ylab="price")

y=lm(price2~bsqft2)

qqplot(bsqft2,price2, xlab="building (FT)", ylab="price",main = "QQPlot")
abline(y)

plot(y, pch=20, cex=.5)

plot(bsqft,price)

#Plot diagnostic plots and analyze
#Use boxcox to determine transformation
#Afterwards plot diagnostic plots and analyze

#7
str(data)
fit= lm(data$price~data$bsqft+data$lsqft)
yhat=fit$fitted.values
e=fit$residuals
fitsum=summary(fit)$coefficients

(3.023863*10^2-(-6.118621*10^(-4)))/sqrt(3.041265^2+8.343616*10^(-4))

#p-value = 1
#don't reject h0

#Use lm function
#Find fitted values by calling them from the lm
#From the summary, we can find the betas and variance and plug into equation

#8

newx=data.frame(data$bsqft,data$price)
splitcounty=split(data, data$county)
#Subset data by bsqft and price, then subset counties with split function
l3=lapply(splitcounty, function(x) lm(price~bsqft, data=x))
#Lapply to loop lm on splitcounty
plot(1, type="n", ylim=range(data$price, na.rm = TRUE), 
     xlim=range(data$bsqft, na.rm = TRUE))
plot(data$price~data$bsqft, xlab="Building size (Ft)",ylab="Price",
     main = "Bsqft vs Price for Seperate Counties")
sapply(1:9, function(i) abline(coef(l3[[i]]), col=i))
legend("bottomright", names(table(data$county)),col=1:9,lty=rep(1,3), cex=.7)
#plot the graph, use sapply to loop abline on the coefficient from the lapply

#9
mean3=aggregate(price~city,data, function(x) (mean(x)))
subsetdata2=data[c(1,2)]
subsetdata3=left_join(subsetdata2, mean3)

data2=distinct(subsetdata3, .keep_all = FALSE)
arrange(data2, county)

#salespercity=t(t(table(data$city)))
salespercity=as.data.frame(table(data$city))
#salespercity=salespercity[,-2]
colnames(salespercity)= c("city","sales")
data$county=as.character(data$county)

city=data[,c(1,2)]
city=arrange(city,city)
salespercity=left_join(city,salespercity)
salespercity=left_join(salespercity, mean3)
salespercity=salespercity[!duplicated(salespercity),]

data3=salespercity %>% group_by(county) %>% top_n(n=3,wt=sales)

treemap(data3, index=c("county","city"),vSize="sales",vColor="price",
        border.col=c("grey70","grey90"), frontsize.title = 18, fontsize.labels=c(15,12),
        align.labels=list(c("left","top"), c("right","bottom")),
        title="Treemap of top cities", type="value")

#https://dplyr.tidyverse.org/reference/join.html
#https://www.rdocumentation.org/packages/dplyr/versions/0.7.3/topics/top_n
#https://www.rdocumentation.org/packages/dplyr/versions/0.7.3/topics/distinct

#10
ymd(data$year)
SFdata = data %>% filter(city == "San Francisco")

SFdata$long2<-round(SFdata$long,2)
SFdata$lat2<-round(SFdata$lat,2)
long3 <- range(SFdata$long2, na.rm=TRUE)
lat3 <- range(SFdata$lat2, na.rm=TRUE)

SFdata$longF=factor(SFdata$long2, levels=seq(long3[1],long3[2], .01))
SFdata$latF=factor(SFdata$lat2, levels=seq(lat3[1],lat3[2], .01))
mat1=table(SFdata$longF,SFdata$latF)
image(mat1)

#https://campus.datacamp.com/courses/free-introduction-to-r/chapter-4-factors-4?ex=4

SFagg=aggregate(price~longF+latF, SFdata,function(x) c(mean(x),length(x)), drop=FALSE)
mat2=daply(SFagg, .(longF,latF), function(x) x$price)
mat3=daply(SFagg, .(longF,latF), function(x) length(x))

SFagg2=SFagg$price[,2]
mat3=matrix(SFagg2,nlevels(SFdata$longF),
                    nlevels(SFdata$latF),byrow=TRUE)
mat3[mat3 == 0] <- NA

image(x=as.numeric(levels(SFdata$longF))+.034,y=as.numeric(levels(SFdata$latF)), z=mat2,
      xlab="Longitude",ylab="Latitude", main = "Heatmap of San Francisco for Price")

image(x=as.numeric(levels(SFdata$longF))+.034,y=as.numeric(levels(SFdata$latF)), z=mat3,
      xlab="Longitude",ylab="Latitude", main = "Heatmap of San Francisco for Number of Houses")

CA_boarder=map('county','california,san francisco')
CA_boarder$x
CA_boarder$y
#plot(1,type="n",xlab="",ylab="", xlim=range(CA_boarder$x, na.rm=TRUE), ylim=range(CA_boarder$y, na.rm=TRUE))
lines(CA_boarder$x,CA_boarder$y)
points(CA_boarder$x,CA_boarder$y)

#Discussion
#Collabated with Jared Yu