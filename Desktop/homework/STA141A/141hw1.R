data=readRDS("~/Desktop/RStudio/Data141a/college_scorecard_2013.rds")
library(tidyverse)

##1

nrow(data)

length(data$main_campus)
#Two different ways to find the length of the dataset, 
#one is by nrow and the other is finding the length of a single sub-data point
sum(data$main_campus)
#After finding the length, there maybe differing colleges recored, 
#if the length equals the number of colleges, we use the sum function

##2
table1 <- sapply(data, class)
dmy<- 'integer'
sapply(data, class)==dmy
(sapply(data,class)==dmy)[sapply(data, class)==dmy]

table1==dmy
table1[table1==dmy]
names(table1[table1==dmy])



##3
is.na(data)
dataNA=colSums(is.na(data))
which.max(dataNA)
#First we remove the NA values with is.na function, after we create a new dataset without the NA values
#The which function finds the index that has the max NA values.

NA_counts<-colSums(is.na(data))

sum(NA_counts)

hist(NA_counts)
table(NA_counts)
which(NA_counts==490)

##4

ownership1 = data %>% filter(ownership == "Public")
count(ownership1[1])

ownership2 = data%>% filter(ownership == "Nonprofit")
count(ownership2[1])

ownership3 = data %>% filter(ownership == "For Profit")
count(ownership3[1])

ownership4 = data %>% filter(ownership != "Public")
count(ownership4[1])

#First we filter out a public and nonpublic and count them.

table(data$ownership=="Public")
table(factor(data$ownership=="Public",labels=c("Not Public","Public")))


degree=as.numeric((data$highest_degree))
ownership=as.numeric((data$ownership))

table(data$ownership,data$highest_degree)

#From there, we can count the degrees for public against nonpublic

ggplot(ownership1, aes(x=highest_degree, y= ownership, col = highest_degree))+geom_col()
ggplot(ownership4, aes(x=highest_degree, y= ownership, col = highest_degree))+geom_col()

props <- prop.table(table(data$ownership,data$highest_degree),margin=1)
prop.table(table(data$ownership,data$highest_degree),margin=2)

data$IsPublic<-factor(data$ownership=="Public",labels=c("Not Public","Public"))

dim(data)

props <- prop.table(table(data$IsPublic,data$highest_degree),margin=1)
mosaicplot(props)
mosaicplot(props, color = c("red","blue","green","purple","yellow" ), shade = FALSE,
           main = "Proportions of High Degree Per College Type",
           xlab = "High Degree Awarded", ylab = "Ownership", las=2)

#Next we plot the data into graphs, Moasaic and barplots relay the same information
#However, Mosaic gives all the data on the same graph.

mat1<-is.na(data)
mat2<-!is.na(data)
dim(mat1)
dim(mat2)

sum(mat1)/(3312*51)
sum(mat2)/(3312*51)

#Use the sum function to find the number of missing valeus then divide by the overall

##5
undergrad_mean = mean(data$undergrad_pop, na.rm = TRUE)
undergrad_median = median(data$undergrad_pop, na.rm = TRUE)

#First we calculate the mean and median using mean and median function

quantile(data$undergrad_pop, na.rm = TRUE)
quantile(data$undergrad_pop, na.rm=TRUE, prob=seq(0,1,length = 11))

#https://stackoverflow.com/questions/26273892/
#r-splitting-dataset-into-quartiles-deciles-what-is-the-right-method

#Next we find the quantile and decile with the quantile function, and for decile,
#We need to specify the probabilties.

density1 = density(data$undergrad_pop, na.rm = TRUE)
probs = quantile(data$undergrad_pop, na.rm = TRUE)

x = is.na(data$undergrad_pop)

y=data[x == FALSE,]
table(is.na(y$undergrad_pop))
table(x)

dataomit=na.omit(data)
hist(dataomit$undergrad_pop) +abline(v = undergrad_mean, col = "red")  
  +abline(v = undergrad_median, col = "blue")

hist(dataomit$undergrad_pop)+
  abline(v=quantile(dataomit$undergrad_pop, c(.25,.5,.75)), col = "brown" )
hist(dataomit$undergrad_pop)+
  abline(v=quantile(dataomit$undergrad_pop, prob=seq(0,1,length = 11)), col = "purple" )

hist(dataomit$undergrad_pop, main = "Histogram of Undergrad Population") 
abline(v=undergrad_mean, col = "red", lwd=3, lty=2)  
abline(v=undergrad_median, col = "blue")
abline(v=quantile(dataomit$undergrad_pop, c(.25,.5,.75)), col = "green" )
abline(v=quantile(dataomit$undergrad_pop, prob=seq(0,1,length = 11)), col = "purple" )

#We omit the NA values since changing them to 0 or any value could skew the data result/graph
#Afterwards, we create ablines for the mean.median,quantile, and decile.

#https://www.r-bloggers.com/quartiles-deciles-and-percentiles/

##6
top5= data[data$state %in% c("CA","TX", "NY", "IL", "FL"),]
topcollege5 = droplevels(top5$state)

boxplot(avg_sat~topcollege5, data = top5, main = "Average SAT score VS Top 5 Populous Schools")
#legend(400,400,legend = c(""), lty = 1:5, col = c(""))

boxplot((undergrad_pop+grad_pop)~topcollege5, data=top5, main = "")
boxplot(tuition~topcollege5, data=top5, main = "Top 5 Earning Schools")

plot(density(top5$tuition[top5$state == "CA"], na.rm = T))
lines(density(top5$tuition[top5$state == "TX"], na.rm = T), lty = 2, col = "red")
lines(density(top5$tuition[top5$state == "NY"], na.rm = T), lty = 3, col = "blue")
lines(density(top5$tuition[top5$state == "IL"], na.rm = T), lty = 4, col = "green")
lines(density(top5$tuition[top5$state == "FL"], na.rm = T), lty = 5, col = "purple")

par(mfrow = c(2, 2),mar=c(1,1,1,1))
hist(top5$tuition[top5$state == "CA"], col = "yellow", main = " ")
hist(top5$tuition[top5$state == "TX"], col = "blue", main = " ")
hist(top5$tuition[top5$state == "NY"], col = "red", main = " ")
hist(top5$tuition[top5$state == "IL"], col = "green", main = " ")
hist(top5$tuition[top5$state == "FL"], col = "pink")

#California, Texas, New York, Illinois, Florida are given as the 5 most populous states
#Droplevels to remove data points which are not in use
#Use boxplot to show the top populous states and their tuition

##7
data_satavg=data[,c(6,14)]
which.max(data$avg_sat)
data[105,]
#Using the which function to find the index where the avg_sat is the largest, 
#then calling it in the data

data_openpop=data[,c(6,5,15)]
which.max(data$undergrad_pop)
data[2371,]
#Using the which function to find the index where the undergrad_pop is the largest, 
#then calling it in the data
data_zipfamily=data[,c(6,9,13,29)]
a=data %>% filter(ownership == "Public")
b=data %>% filter(ownership == "Public") %>% summarize(which.min(avg_family_inc))
a[348,]
#subset the data to filter only public, then from the subset data, use which to find the smallest
#avg_family_inc index, call the index from the subset data to find the zip code.

data_gradpop=data[,c(6,5,15,16)]
which.max(data$grad_pop)
data[248,]
#Using the which function to find the index where the grad_pop is the largest, 
#then calling it in the data

##8
newdata1=data %>% filter(ownership == "For Profit", primary_degree == "Bachelor") 
ggplot(newdata1, aes(x=revenue_per_student, y=spend_per_student))+
  geom_point()+geom_smooth(method = "lm", se = FALSE, lty=2)

newdata1$total_net_income = newdata1$undergrad_pop*
  (newdata1$revenue_per_student - newdata1$spend_per_student)

data2 <- newdata1[order(newdata1$total_net_income) , ]
data2 <- head(newdata1[order(-newdata1$total_net_income, newdata1$main_campus) ,],5)
data2$total_net_income
data2$name
data3=cbind(data2$name, (data2$total_net_income) )

barplot(data2$total_net_income, col = c("red","blue","green","yellow","purple"), 
        names.arg= c("Phoenix","Ashford","Kaplan","DeVry","Grand Canyon"), 
        ylab = "USD in million dollars", main = "Top 5 Earning Schools")

#Subset the data with ownership and primary_degree
#Afterwards, plot the graph using the new data as the data frame.
#Next to create the total_net_income, we create the data using 
#Revenue_per_student subtracted by spend_per_student and multple the difference by undergrad_pop
#Plot the graph with total_net_income for a barplot


##9
data$avg_sat
data$admission

plot(data$avg_sat,data$admission)

data$group = data %>% filter(avg_sat > 1200 & admission < .5)
data$group <- factor(c(data$avg_sat>1200, data$admission < .4), labels=c("above","below"))

group<-(data$avg_sat > 1200) & (data$admission < .4)
group2<-ifelse(is.na(group),FALSE,group)

props2 <- prop.table(table(data$avg_sat > 1200,data$admission < .4),margin=1)
mosaicplot(props2)
  mosaicplot(props2, color = c("red","blue" ), shade = FALSE,
           main = "Average SAT score vs Admission",
           xlab = "Average SAT score above 1200", ylab = "Admission below .4", las=2)

ggplot(data, aes(x=avg_sat, y=admission, col = group2))+geom_point()+
  geom_smooth(method = "lm", se = FALSE, col = "blue",lty=2)

ggplot(data, aes(avg_sat, med_10yr_salary, col = group2))+geom_point()+
  geom_smooth(method = "lm", se = FALSE, col = "blue",lty=2)

ggplot(data, aes(x=avg_sat,y=race_white+race_asian, col = group2), 
       main = "Average SAT score vs Race:White+Asian")+geom_boxplot(aes(x=group2))

ggplot(data, aes(avg_sat, data$grad_pop, col = group2), 
       main =" Graduate Population vs Average SAT" )+geom_boxplot(aes(x=group2))

sum(dataomit$grad_pop)/sum(dataomit$grad_pop+dataomit$undergrad_pop)

#First subset the data by avg_sat and admission, however doing just that will not help
#To provide a better subset, we will require conditions on the data we're working on
#Then plot the data using the subsetted data, continue using the subsetted data for the other graphs

##10
ggplot(data, aes(x=avg_family_inc, y=avg_10yr_salary))+geom_point()+
  geom_smooth(method = "lm", se = FALSE, lty=2)

data4= data[-c(1789,2030.1956,682,1331,891,1085,1577,1241,1337,2180,1635,2204),]

ggplot(data4, aes(x=avg_family_inc, y=avg_10yr_salary))+geom_point()+
  geom_smooth(method = "lm", se = FALSE, lty=2)

#Plot the data using the original data frame, afterwards, find the outliers and remove them
#replot the data