data1=read.csv("~/Desktop/RStudio/Data141a/color_combos.csv")
data2=read.csv("~/Desktop/RStudio/Data141a/properties.csv")
#Command,shift,c

simulate_monopoly=function(n,d){
  location=numeric(n+1)
  location[1]=0
  #Create a vector for location
  communitychest_location=c(2,17,33)
  chance_location=c(7,22,36)
  
  communitychest=sample(1:16)
  communityindex=c(1:16)
  #Create the community chest deck
  chance=sample(1:16)
  chanceindex=c(1:16)
  #Create the chance deck  
  tax=c(4,38)
  utilty=c(12,28)
  jail=10
  
  railroad=c(5,15,25,35)
  
  roll1=sample(1:d, n, replace=TRUE)
  roll2=sample(1:d, n, replace=TRUE)
  #Create dice rolls
  for(i in 1:n){
    doublecount=0
    if(roll1[i]==roll2[i]){
      doublecount=doublecount+1
    }
    else if(roll1[i] != roll2[i]){
      doublecount=0
    }
    if(doublecount == 3){
      location[i+1]=10
      doublecount=0
    }
    location[i+1]=(location[i]+roll1[i]+roll2[i]) %% 40
    
    if(location[i]==30){
      location[i]=10
      next}
    #if(location[i]==30) {print("move")}
  }
  #When the two dices equal each other in value, increase the double count
  #When the double count reachs three, the player is sent to jail, and return double count to zero
  #Otherwise, when the dices are not equal, the double count value is returned to zero
  
  if(location[i+1] %in%  communitychest_location){
    if(communitychest[communityindex==1]) {location[i+1]= 0}
    if(communitychest[communityindex==2]) {location[i+1]= 10}
    communitychest=c(communitychest[-1],communitychest[1])
  }
  #When the community chest has the card index 1 drawn, the player is sent to go spot
  #As well, when the card index 2 is drawn, the player is sent to jail
  #Last equation updates the deck after a card is pulled, then it is sent to the bottom of the deck
  
  if(location[i+1] %in% chance_location){
    if(chance[chanceindex==1]) {location[i+1]= 0}
    if(chance[chanceindex==2]) {location[i+1]= 10}
    if(chance[chanceindex==3]) {location[i+1]= 2}
    if(chance[chanceindex==4]) {location[i+1]= 24}
    if(chance[chanceindex==5]) {location[i+1]= 39}
    if(chance[chanceindex==6]) {location[i+1]= 5}
    if(chance[chanceindex==7]) 
    {if(location[i+1] < 5 && location[i+1] > 35) {location[i+1] = 5}
      else if(location[i+1] < 15 && location[i+1] > 5) {location[i+1] = 15}
      else if(location[i+1] < 25 && location[i+1] > 15) {location[i+1] = 25}
      else if(location[i+1] < 35 && location[i+1] > 25) {location[i+1] = 35}
    } 
    if(chance[chanceindex==8]) 
    {if(location[i+1] < 5 && location[i+1] > 35) {location[i+1] = 5}
      else if(location[i+1] < 15 && location[i+1] > 5) {location[i+1] = 15}
      else if(location[i+1] < 25 && location[i+1] > 15) {location[i+1] = 25}
      else if(location[i+1] < 35 && location[i+1] > 25) {location[i+1] = 35}
    } 
    if(chance[chanceindex==9]) 
    {
      #print("test1")
      if(location[i+1] < 12 | location[i+1] > 28) 
      {
        location[i+1] = 12
        #print("test2")
      }
      else
        location[i+1] = 28
    }
    if(chance[chanceindex==10]) {location[i+1]=location[i+1] - 3}
    chance=c(chance[-1],chance[1]) 
  }
  #Similar to the Commmunity chest deck
  #To account for the railroad, the location is checked within the range and then moved after
  #Similar to railroad is done on the ulity
  
  #return(factor(location, 0:39))
  location
}
#http://stat.ethz.ch/R-manual/R-patched/library/base/html/Paren.html
#https://stackoverflow.com/questions/12614953/how-to-create-a-numeric-vector-of-zero-length-in-r


estimate_monopoly=function(n,d){
  tablesim=as.data.frame(table(simulate_monopoly(n,d)) )
  tablesim$Freq=tablesim$Freq/n
  tablesim
}
#The estimate monopoly calls the simular monopoly function
#The data is chanced into a data.frame and calling the freq from the data frame and dividing from the whole number


tablesim2=as.data.frame(table(simulate_monopoly(10000,2)))
names(tablesim2)=c("location","Freq")
tablesim2$Freq3=estimate_monopoly(10000,3)$Freq
tablesim2$Freq4=estimate_monopoly(10000,4)$Freq
tablesim2$Freq5=estimate_monopoly(10000,5)$Freq
tablesim2$Freq6=estimate_monopoly(10000,6)$Freq
#Use estimate monopoly and change the d(number of dice sides)

plot(tablesim2$Freq3,type="n",xlab="location",ylab="Freq",xaxt="n", main="Location vs Freq")
lines(tablesim2$Freq3, col="red")
lines(tablesim2$Freq4, col="green")
lines(tablesim2$Freq5, col="orange")
lines(tablesim2$Freq6, col="purple")
legend("topright", c("3-sided dice","4-sided dice","5-sided dice","6-sided dice"), col=c("red","green","orange","purple"))
axis(side = 1,at=tablesim2$location, labels=tablesim2$location, cex.axis=.4)

#Plot the line plot, however, the plot is hard to distringuish.

matrix=t(cbind(tablesim2$Freq3,tablesim2$Freq4,tablesim2$Freq5,tablesim2$Freq6))
library(fields)
image(matrix, axes=FALSE, main="Distribution for Locations")
mtext(text=c(paste("",0:39)), side=2, outer=FALSE, line=0.3, at=seq(0,1,0.025641), las=1, cex=0.8)
mtext(text=c(paste(3:6,"sided")), side=1, line=0.3, at=seq(0,1,0.3), las=1, cex=0.8)
par(new=T)
image.plot(matrix, legend.only=FALSE, axes=FALSE, main="Distribution for Locations")

#Plot heatmap to better represent the data

#Colobrated with Linlin Liu

#https://stackoverflow.com/questions/12895783/r-language-mtext-not-working-with-image-
#plot-array

#https://stackoverflow.com/questions/4785657/r-how-to-draw-an-empty-plot
#https://stackoverflow.com/questions/11775692/how-to-specify-the-actual
#-x-axis-values-to-plot-as-x-axis-ticks-in-r

jailsd = replicate(1000,estimate_monopoly(10000,6)[11,2])
sd(jailsd)

#Create multiple estimate monopoly with replicate then find the standard deviation of them all

simulate_monopoly2=function(n,d,data,rent){
  location=numeric(n+1)
  location[1]=0
  #Create vector
  money=numeric(n+1)
  money[1] = 0
  #Create vector
  communitychest_location=c(2,17,33)
  chance_location=c(7,22,36)
  #Create locations for community chest and chance
  communitycard=c(0,10,rep(NA,14))
  communitychest=sample(communitycard)
  #Create the Community Chest deck and shuffle
  chancecard=c(0,10,11,24,39,5,-1,-1,-2,-3,rep(NA,6))
  chance=sample(chancecard)
  #Create the Chance deck and shuffle
  tax=c(4,38)
  utilty=c(12,28)
  jail=10
  
  railroad=c(5,15,25,35)
  
  roll1=sample(1:d, n, replace=TRUE)
  roll2=sample(1:d, n, replace=TRUE)
  #Create dices
  for(i in 1:n){
    doublecount=0
    if(roll1[i]==roll2[i]){
      doublecount=doublecount+1
    }
    else if(roll1[i] != roll2[i]){
      doublecount=0
    }
    if(doublecount == 3){
      location[i+1]=10
      doublecount=0
    }
    
    location[i+1]=(location[i]+roll1[i]+roll2[i]) %% 40
    
    if(location[i+1]==30){
      location[i+1]=10
    }
    #if(location[i]==30) {print("move")}
    
    #When the two dices equal each other in value, increase the double count
    #When the double count reachs three, the player is sent to jail, and return double count to zero
    #Otherwise, when the dices are not equal, the double count value is returned to zero
    
    
    if(location[i+1] %in%  communitychest_location){
      communitycard=communitychest[1]
      if(!is.na(communitycard)){
          location[i+1]=communitycard
      }
      communitychest=c(communitychest[-1],communitychest[1])
    }
    #When the deck is drawing cards, the location will move as long as the card is not "NA"
    #When the card is pulled, the location will move to that index, when "10" is pulled from the deck, the location will be 10
    
    if(location[i+1] %in% chance_location){
      chancecard=chance[1]
      chance=c(chance[-1],chance[1])
      if(!is.na(chancecard) ){
        if(chancecard == 0 | chancecard == 5 | chancecard == 10 | 
           chancecard == 11 | chancecard == 24 | chancecard == 39){
          location[i+1]=chancecard
          #print('ClearV')
        }
        if(chancecard == -1){
          rr=function(location){
            railroad=seq(5,45,10)
            location2=min(railroad[railroad>location%%40])%%40
            return(location2)
          }
          location[i+1]=rr(location[i+1])
          #print('Clearb')
        }
        if(chancecard == -2){
          uu=function(location){
            ulity=c(12,28,52)
            location3=min(ulity[ulity>location%%40])%%40
            return(location3)
          }
          location[i+1]=uu(location[i+1])
          #print('Clearz')
        }
        if(chancecard == -3){
          location[i+1]=location[i+1]-3
          #print('Clears')
        }
      }
    }
    #Similar to the Community Chest, when the card is drawn, the location will move to that index
    #When "-1" is pulled, the location will move to the minimum location after taking a modulo
    #Similar is done with ulity
    #When "-3" is drawn the equation just moves the player back 3 spots.
    
    
    if(location[i+1] %in% property){
      #print('Check')
      #Rent_current = data$Rent[match(location[i+1],data$Index)]
      index = which(location[i+1] == property)
       money[i+1]= money[i]-rent[index]
      #money[i+1] = money[i]- Rent_current
     # cat("data index",location[i+1],"\n")
     # cat("Rent_current",Rent_current,"\n")
    }
    #When the location is on the specific color location, the money is loss depending on the rent
    
    #Colobrated with Huong Vu
    
    if(location[i+1] == 4){
      money[i+1] = money[i]
    }
    if(location[i+1] == 38){
      money[i+1] = money[i]-100
    }
    if(location[i+1] + roll1[i]+roll2[i] > 39){
      money[i+1] =money[i]+200
    }
  }
  #When the player lands on ulity, player will lose either 200 or 100 depending on the spot
  #When the player passes "GO", they will gain 200, but will not gain money if they pass using a card or is sent to jail
  
  #return(factor(location, 0:39))
  return(cbind(location,money))
}

temporarymoney=function(data,co){
  #data = data2; co = 'Pink'
  df=subset(data ,data$Color==co)
  dff=simulate_monopoly2(100,6, df)
  money2=sum(dff[[2]])
  return(money2)
}

property = which(data2$Color == "Purple")
rent = data2$Rent[which(data2$Color == "Purple")]
colorpurple=replicate(1000,sum(simulate_monopoly2(100,6,property,rent)[,2]))

property = which(data2$Color == "Light Blue")
rent = data2$Rent[which(data2$Color == "Light Blue")]
colorlblue=replicate(1000,sum(simulate_monopoly2(100,6,property,rent)[,2]))

property = which(data2$Color == "Pink")
rent = data2$Rent[which(data2$Color == "Pink")]
colorpink=replicate(1000,sum(simulate_monopoly2(100,6,property,rent)[,2]))

property = which(data2$Color == "Orange")
rent = data2$Rent[which(data2$Color == "Orange")]
colororange=replicate(1000,sum(simulate_monopoly2(100,6,property,rent)[,2]))

property = which(data2$Color == "Red")
rent = data2$Rent[which(data2$Color == "Red")]
colorred=replicate(1000,sum(simulate_monopoly2(100,6,property,rent)[,2]))

property = which(data2$Color == "Yellow")
rent = data2$Rent[which(data2$Color == "Yellow")]
coloryellow=replicate(1000,sum(simulate_monopoly2(100,6,property,rent)[,2]))

property = which(data2$Color == "Green")
rent = data2$Rent[which(data2$Color == "Green")]
colorgreen=replicate(1000,sum(simulate_monopoly2(100,6,property,rent)[,2]))

property = which(data2$Color == "Blue")
rent = data2$Rent[which(data2$Color == "Blue")]
colorblue=replicate(1000,sum(simulate_monopoly2(100,6,property,rent)[,2]))

colorsd1=cbind(colorpurple,colorlblue,colorpink,colororange,colorred,coloryellow,colorgreen,colorblue)
boxplot(colorsd1, xaxt="n",lab = "Money", xlab= "color", 
        col = c("purple"," sky blue","pink","orange","red","yellow","green","blue"), 
        main = "Boxplot of Money Distribution")

#Call the color's index and the rent
#When using with the simulate monopoly 2 function, it will return only the specific color's rent
#Then plot the boxplot

#Colobrated with Linlin Liu

#http://r.789695.n4.nabble.com/splitting-matrices-td4654535.html

head(sort(c(colorsd1[,1],colorsd1[,2],colorsd1[,3],colorsd1[,4],colorsd1[,5],colorsd1[,6],colorsd1[,7],colorsd1[,8])))
tail(sort(c(colorsd1[,1],colorsd1[,2],colorsd1[,3],colorsd1[,4],colorsd1[,5],colorsd1[,6],colorsd1[,7],colorsd1[,8])))

lines(density(colorsd1[,1]),col="purple")
lines(density(colorsd1[,2]),col="sky blue")
lines(density(colorsd1[,3]), col ="pink")
lines(density(colorsd1[,4]), col="orange")
lines(density(colorsd1[,5]), col="red")
lines(density(colorsd1[,6]), col="yellow")
lines(density(colorsd1[,7]), col="green")
lines(density(colorsd1[,8]), col="blue")

plot(density(colorsd1[,1]),col="purple",xlab="Money",ylab="Freq", xlim=c(-24775,8100),
     main="Density plot of Money Distribution")

#Plot density plot