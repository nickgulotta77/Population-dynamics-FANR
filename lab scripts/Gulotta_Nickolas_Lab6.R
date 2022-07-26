#Exercise 1----
nYears <- 51
r.prey <- 0.05
d.prey <- 0.0005
b.pred <- 0.001
d.pred <- 0.45
N.prey <- rep(NA, nYears)
N.pred <- rep(NA, nYears)
N.prey[1] <- 500
N.pred[1] <- 100

#loop
for(t in 2:nYears) {
  N.prey[t] <- N.prey[t-1] + N.prey[t-1]*(r.prey-d.prey*N.pred[t-1])
  N.pred[t] <- N.pred[t-1] + N.pred[t-1]*(b.pred*N.prey[t-1] - d.pred)
}

#new dataframe for graph
Year<- 0:50
data<- data.frame(Year, N.prey, N.pred)

#rename variables for graph
colnames(data) <- c('Year', 'Prey', 'Predator')

#convert data to long format for plotting
require(tidyr)
data1<-data%>% 
  pivot_longer(c("Prey", "Predator") ,names_to= "Group", values_to= "Abundance")

##plot 1----
require(ggplot2)
p1<-ggplot(data1, aes(x=Year, y=Abundance, group=Group, color=Group)) + 
  geom_point() +
  geom_line() 

p1+theme_classic()

##pred/prey equilibrium part a----

a<-r.prey/d.prey #prey equilibrium
a2<-d.pred/b.pred #predator equilibrium

# part 2
nYears1 <- 51
r.prey1 <- 0.1
d.prey1 <- 0.0005
b.pred1 <- 0.001
d.pred1 <- 0.45
N.prey1 <- rep(NA, nYears1)
N.pred1 <- rep(NA, nYears1)
N.prey1[1] <- 500
N.pred1[1] <- 100

#loop
for(t in 2:nYears1) {
  N.prey1[t] <- N.prey1[t-1] + N.prey1[t-1]*(r.prey1-d.prey1*N.pred1[t-1])
  N.pred1[t] <- N.pred1[t-1] + N.pred1[t-1]*(b.pred1*N.prey1[t-1] - d.pred1)
}

#new dataframe for graph
Year<- 0:50
data2<- data.frame(Year, N.prey1, N.pred1)

#rename variables for graph
colnames(data2) <- c('Year', 'Prey', 'Predator')

#convert data to long format for plotting
require(tidyr)
data3<-data2%>% 
  pivot_longer(c("Prey", "Predator") ,names_to= "Group", values_to= "Abundance")

##plot 2----
require(ggplot2)
p2<-ggplot(data3, aes(x=Year, y=Abundance, group=Group, color=Group)) + 
  geom_point() +
  geom_line() 

p2+theme_classic()

##pred/prey equilibrium part d---
d<-r.prey1/d.prey1 #prey equilibrium
d2<-d.pred1/b.pred1 #predator equilibrium

#Exercise 2----

nYears2 <- 26
ra <- 0.2
rb<-0.3
ka<-250
kb<-200
alphaA<-0.1
alphaB<-0.1
N.spA <- rep(NA, nYears2)
N.spB <- rep(NA, nYears2)
N.spA[1] <- 200
N.spB[1] <- 50



#loop
for(t in 2:nYears2) {
  N.spA[t] <- N.spA[t-1] + ra*N.spA[t-1]*(ka-N.spA[t-1]-alphaB*N.spB[t-1])/ka
  N.spB[t] <- N.spB[t-1] + rb *N.spB[t-1]*(kb-N.spB[t-1] - alphaA*N.spA[t-1])/kb
}

#new dataframe for graph
Year<- 0:25
data3<- data.frame(Year, N.spA, N.spB)

#rename variables for graph
colnames(data3) <- c('Year', 'Species_A', 'Species_B')

#convert data to long format for plotting
require(tidyr)
data4<-data3%>% 
  pivot_longer(c("Species_A", "Species_B") ,names_to= "Group", values_to= "Abundance")

##plot 3----
require(ggplot2)
p3<-ggplot(data4, aes(x=Year, y=Abundance, group=Group, color=Group)) + 
  geom_point() +
  geom_line() 

p3+theme_classic()


#species equilibrium----
spAeq<-(ka-(alphaB*kb)/(1-(alphaA*alphaB))) #species A equilibrium

spBeq<-(kb-(alphaA*ka)/(1-(alphaA*alphaB))) #species B equilibrium

#part 2

nYears3 <- 26
ra1 <- 0.2
rb1<-0.3
ka1<-250
kb1<-200
alphaA1<-0.1
alphaB1<-ka/kb
N.spA1 <- rep(NA, nYears3)
N.spB1 <- rep(NA, nYears3)
N.spA1[1] <- 200
N.spB1[1] <- 50


#loop
for(t in 2:nYears3) {
  N.spA1[t] <- N.spA1[t-1] + ra1*N.spA1[t-1]*(ka1-N.spA1[t-1]-alphaB1*N.spB1[t-1])/ka1
  N.spB1[t] <- N.spB1[t-1] + rb1 *N.spB1[t-1]*(kb1-N.spB1[t-1] - alphaA1*N.spA1[t-1])/kb1
}

#new dataframe for graph
Year<- 0:25
data5<- data.frame(Year, N.spA1, N.spB1)

#rename variables for graph
colnames(data5) <- c('Year', 'Species_A', 'Species_B')

#convert data to long format for plotting
require(tidyr)
data6<-data5%>% 
  pivot_longer(c("Species_A", "Species_B") ,names_to= "Group", values_to= "Abundance")

##plot 4----
require(ggplot2)
p4<-ggplot(data6, aes(x=Year, y=Abundance, group=Group, color=Group)) + 
  geom_point() +
  geom_line() 

p4+theme_classic()
