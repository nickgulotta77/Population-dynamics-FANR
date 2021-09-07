#required package
#install.packages("ggplot2")
#install.packages("dplyr")

#Exercise 1----
years1 <- 2005:2024 ## All years
nYears1 <- length(years1) ## Number of years
nYearsWithData <- 8 ## Years with data
nYearsWithoutData <- nYears1-nYearsWithData ## nYears without data
pythons <- c(10, 25, 63, 156, 391, 977, 2441, 6104, ## Python counts
             rep(NA, nYears1-nYearsWithData))


data<-data.frame(years1, pythons)#create data frame for loop
## Question 1.. calculate lambda, growth pattern is geometric growth 
for(i in 2:8){
  #i=2
  lambda<-round(data[i,2]/data[i-1,2],2)
  data[i,3]<-lambda
  
}

#question 2
r<-round(lambda[1]-1,2) #growth rate(r)

#question 3
b<-2.0 #birth rate(b)
d<- b-r #death rate (d)

#question 4

h<-r #sustainable harvest/yield is equal to the growth rate


#Question 5
for(i in 8:19){
  #i=8
  data[i,4]<-data[i,2]*r
  data[i+1,2]<-data[i,2]+data[i,2]*r-data[i,4]
}
data


# for loop that calculates remeaining lambda values
for(i in 9:20){
  #i=2
  lambda<-round(data[i,2]/data[i-1,2],2)
  data[i,3]<-lambda
  
}

require(dplyr)
data<-data %>% mutate(years1, pythons, V3, V4) %>%
  rename(
    Lambda =  V3 ,
    PythonsHarvested = V4 
  )


##plot of abundance of pythons---- 
require(ggplot2)
plot1<-ggplot(data= data, aes(x=years1, y=pythons)) +
  geom_point()+
  geom_line() +
scale_x_continuous(name="Year", breaks = seq(2005, 2025,2), 
                   minor_breaks =seq( 2003,2024,1))
                   
  plot1 + ylab( "Abundance of pythons") + theme_classic() #plot 1


  
#Exercise 2----
  
  Time<-0:40
  NWNH<-rep(NA,41) #N w/no harvest
  NWNH[1]<-100 # assign first N
  SH<-rep(NA,41) # blank variable for sustainable harvest
  
  rmax<-0.32 
  k<-2000 #carrying capacity
  
  data1<-data.frame(Time,NWNH,SH)
  
  #Question 1
  for( i in 1:nrow(data1)){
    #i=1
    data1[i,3]<-data1[i,2]*rmax*(1-data1[i,2]/k)
    data1[i+1,2]<-data1[i,2] + data1[i,3]
  }
  
  
  
  ##plot of abundance without harvest of quail---- 
  require(ggplot2)
  plot2<-ggplot(data= data1, aes(x=Time, y=NWNH)) +
    geom_point()+
    geom_line() +
    scale_x_continuous(name="Time", breaks = seq(0, 40,5), 
                       minor_breaks =seq(0,41,1))
  
  plot2 + ylab( "Abundance (without harvest)") + theme_classic() #plot 2
  
  
  
##plot of sustainable harvest---- 
  require(ggplot2)
  plot3<-ggplot(data= data1, aes(x=NWNH, y=SH)) +
    geom_point()+
    geom_line() +
    scale_x_continuous(name="Abundance", breaks = seq(0, 2100,250), 
                       minor_breaks =seq(0,2100,100))
  
  plot3 + ylab( "Harvest") + theme_classic() #plot 3
  

  
  #Question 3
  NMSY<-k/2 # abundance at max sustainable yield
  
  #Question 4
  MSY<-rmax*k/4 # msy or max number of individuals you can harvest
  
  #Question 5
  
require(dplyr)
data2<-data1 %>% mutate(Time, NWNH, SH) %>%
  rename(
  NWHarvest =  NWNH ,
  HarvestPlan = SH  
  )
 
  for( i in 1:nrow(data2)){
    #i=1
    if(i <11){
      data2[i,3]<-0
    }else{
      data2[i,3]<-data2[i,2]*rmax*(1-data2[i,2]/k)
    }
    data2[i+1,2]<-data2[i,2]+data2[i,2]*rmax*(1-data2[i,2]/k) - data2[i,3]
  }
  data2<-data2[-nrow(data2),]
  

#Exercise 3----
  
  
#Question 1
  N<-seq(from=10, to=200, by=10)
  survival<-rep(NA,20)
  beta0<-0.95
  beta1<-0.003
  IA<-200
  data3<-data.frame(N, survival)
  
  
  for(i in 1:nrow(data3)){
    #i=1
    data3[i,2]<-beta0-beta1*data3[i,1]
  }
  
  
  ##plot of survival probability---- 
  require(ggplot2)
  plot4<-ggplot(data= data3, aes(x=N, y=survival)) +
    geom_point()+
    geom_line() +
    scale_x_continuous(name="Abundance", breaks = seq(0, 200,20), 
                       minor_breaks =seq(0,200,10))
  
  plot4 + ylab( "Survival probability") + theme_classic() #plot 4
  

  
  #Question 2
  HOPT<-seq(from=10, to=150, by=10)
  AAH<-rep(NA,15)
  PHS<-rep(NA,15)
  AF<-rep(NA,15)
  
  data4<-data.frame(HOPT,AAH,PHS,AF)
  
  
  for(i in 1:nrow(data4)){
    #i=1
    data4[i,2]<-IA - data4[i,1]
    data4[i,3]<- beta0 - beta1 * data4[i,2]
    data4[i,4]<- data4[i,2]*data4[i,3]
  }
  
  
  #Question 3
  ##plot of final abundance vs harvest---- 
  require(ggplot2)
  plot5<-ggplot(data= data4, aes(x=HOPT, y=AF)) +
    geom_point()+
    geom_line() +
    scale_x_continuous(name="Harvest option abundance", breaks = seq(0, 150,10), 
                       minor_breaks =seq(0,150,10)) + 
    scale_y_continuous(name = "Final abundnace", breaks = seq (40, 75, 5), 
                      minor_breaks= seq(40, 75, 5))
  
  plot5 + ylab( "Final abundance") + theme_classic() #plot 5
  
  
  #Question 4.. Answered in excel. If lower population then there will be a greater surival. 
  a<-beta0-beta1*IA
  NoHarvest<-IA*a

  
  #Q5
  NumberHarvested<-data4[which(data4$AF== max(data4$AF)),1]*2

  NumberHarvested
