nSites <- 16 #number of sites
nYears <- 41 #number of years
gamma <- 0.3 ## Colonization probability
epsilon <- 0.2 ## Local extinction probability

O <- psi <- matrix(NA, nYears, nSites)
O[1,1:2] <-1
O[1,3:16] <-0

for(t in 2:nYears) {
  psi.t <- O[t-1,]*(1-epsilon) + (1-O[t-1,])*gamma
  O[t,] <- rbinom(nSites, size=1, prob=psi.t)
}

O


nOccupied <- rowSums(O)
nOccupied

PrO <- nOccupied / nSites
PrO

#convert to data frame
Years<-0:40
data1<-data.frame(Years, PrO)

#plot 
p1<-ggplot(data1, aes(x=Years, y=PrO))+
  geom_point() +
  geom_line() + 
  scale_y_continuous("Proportion of occupied sites") +
  theme_classic()

p1
#exercise 2


lambda <- c(1.05, 0.95)
pi <- matrix(c(
0, 0.05,
0.04, 0), nrow=2, ncol=2, byrow=TRUE) #matrix for meta
nPatches <- 2
nYears <- 41


n <- matrix(NA, nYears, nPatches) #matrix for meta
n[1,] <- c(500,500)

#meta for loop
for(t in 2:nYears) {
  n[t,1] <- n[t-1,1]*lambda[1]*(1-pi[1,2]) +
    n[t-1,2]*lambda[2]*pi[2,1]
  n[t,2] <- n[t-1,2]*lambda[2]*(1-pi[2,1]) +
    n[t-1,1]*lambda[1]*pi[1,2]
}

#isolated pop
N1 <- rep(NA, nYears)
N2 <- rep(NA, nYears)
N1[1]<- 500 #first year abundance
N2[1]<-500
lambda1<-1.05
lambda2<-0.95
#isolated loop
for(t in 2:nYears) {
  N1[t]<-N1[t-1]*lambda1
  N2[t]<-N2[t-1]*lambda2
  
}


#lambda for meta
lambda.it <- n[-1,] / n[-nYears,]

#make data frame for plot
Years<-0:40 #make years 0 to 40
N<-data.frame(Years, N1,N2,n)
#rename variables for graph
colnames(N) <- c('Year', 'N1_isolated', 'N2_isolated', 'N1_meta', 'N2_meta')
#convert data to long format for plotting
require(tidyr)
data<-N%>% 
  pivot_longer(c('N1_isolated', 'N2_isolated', 'N1_meta', 'N2_meta') ,names_to= "Group", values_to= "Abundance")

##plot 1----
require(ggplot2)
p2<-ggplot(data, aes(x=Year, y=Abundance, group=Group, color=Group)) + 
  geom_point() +
  geom_line() 

p2+theme_classic()

