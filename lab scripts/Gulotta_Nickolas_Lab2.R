#Exercise 1----
year <- 2015:2025 # A vector for years
year 


nYears <- length(year)
nYears


N <- rep(NA, nYears) #create empty variable

N[1]<- 100 #first year abundance

B<- 10 #number of births

D<- 20 #deaths

b<- B/100 # birth rate

d<- D/100 # death rate

r<- b-d

lambda<- 1 + r


for(t in 2:nYears) {
  N[t] <-N[t-1]  + N[t-1]*r 
}

N

##plot 1----
plot(year, N, type="l", xlab="Year", ylab="Abundance",
     main="Predicted Wolf Population Size(Geometric growth)")


#Exercise 2a----

year1<-2012:2021


nYears1 <- length(year1)
nYears1


N1 <- rep(NA, nYears1) #create empty variable

N1[1]<-7.00 # 2012 population size

r1<- (7.00/6.00)^(1/13)-1

for(t in 2:nYears1) {
  N1[t] <-N1[t-1]  + N1[t-1]*r1 
}

N1



##plot 2a----
plot(year1, N1, type="l", xlab="Year", ylab="Abundance",
     main="Predicted Human Population Size(Geometric growth)")




#exercise 2b actual abundance----

year2<-1999:2012


nYears2 <- length(year2)
nYears2


N2 <- rep(NA, nYears2) #create empty variable

N2<-c(6,6.12,6.24,6.35,6.45,6.55,6.65,
      6.74,6.81,6.87,6.92,6.96,6.99,7.02) #actual abundance



##plot for actual abundance----

plot(year2, N2, type="l", xlab="Year", ylab="Abundance",
     main="Actual Abundance of Humans")


#Exercise 2b lambda values----
year3<-2000:2012


nYears3 <- length(year3)
nYears3


N3<-c(6.12,6.24,6.35,6.45,6.55,6.65,
      6.74,6.81,6.87,6.92,6.96,6.99,7.02) #actual abundance




lambda1 <- rep(NA, nYears3) #create empty variable

lambda1[1]<-6.12/6 #first lambda value

for(t in 2:nYears3) {
  lambda1[t] <-N3[t] / N3[t-1]
}

lambda1

##plot for 2b lambda values----
plot(year3, lambda1, type="l", xlab="Year", ylab="Lambda",
     main="Change in Lambda over time")



#Exercise 2b geometric growth----

Years4<-1999:2012
nYears4 <- length(Years4)
nYears4

N4<-c(6,6.12,6.24,6.35,6.45,6.55,6.65,
      6.74,6.81,6.87,6.92,6.96,6.99,7.02) #actual abundance

r1<-0.009

N5<-rep(NA, nYears4)

N5[1]<-6

for(t in 2:nYears4) {
  N5[t] <- N4[t-1] + N4[t-1]*r1
}

N5

##plot for 2b geometric growth----
plot(Years4, N5, type="l", xlab="Year", ylab="Abundance",
     main="Predicted Geometric growth")

#Exercise 2b logistic growth----

Years5<-1999:2012

nYears5 <- length(Years5)
nYears5

N6<-c(6,6.12,6.24,6.35,6.45,6.55,6.65,
      6.74,6.81,6.87,6.92,6.96,6.99,7.02) #actual abundance

rmax<-0.04
K<-7.9


N7<-rep(NA, nYears5)

N7[1]<-6

## for loop
for(t in 2:nYears5) {
  N7[t] <- N6[t-1] + N6[t-1]*rmax*(1 - N6[t-1]/K)
}
N7

##plot for 2b logistic growth----
plot(Years5, N5, type="l", xlab="Year", ylab="Abundance",
     main="Predicted Logistic growth")
