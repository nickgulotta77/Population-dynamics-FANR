#packages needed
#install.packages("tidyr")
#install.packages("ggplot2")

#exercise 1----
r <- -0.2
sigma <- 10
nYears <- 31
extinctionThreshold <- 20

#geometric growth for N (with no stochasticity)
N <- rep(NA, nYears) ## Population size
N[1] <- 500 ## Initial population size
for(t in 2:nYears) {
  N[t] <- (N[t-1] + N[t-1]*r)*(N[t-1]>extinctionThreshold)
}

## Repeat the next steps 10 times to do 10 simulations
## Or, do a 'nested for loop' and save each simulation (harder)
N1 <- rep(NA, nYears) ## Population size
X <- rep(NA, nYears) ## Random variable for environmental stochasticity
N1[1] <- 500 ## Initial population size
for(t in 2:nYears) {
  X[t-1] <- rnorm(n=1, mean=0, sd=sigma)
  N1[t] <- (N1[t-1] + N1[t-1]*r + X[t-1])*(N1[t-1]>extinctionThreshold)
}

#second simulation
N2 <- rep(NA, nYears) ## Population size
X1 <- rep(NA, nYears) ## Random variable for environmental stochasticity
N2[1] <- 500 ## Initial population size
for(t in 2:nYears) {
  X1[t-1] <- rnorm(n=1, mean=0, sd=sigma)
  N2[t] <- (N2[t-1] + N2[t-1]*r + X1[t-1])*(N2[t-1]>extinctionThreshold)
}


#third simulation

N3 <- rep(NA, nYears) ## Population size
X2 <- rep(NA, nYears) ## Random variable for environmental stochasticity
N3[1] <- 500 ## Initial population size
for(t in 2:nYears) {
  X2[t-1] <- rnorm(n=1, mean=0, sd=sigma)
  N3[t] <- (N3[t-1] + N3[t-1]*r + X2[t-1])*(N3[t-1]>extinctionThreshold)
}


# fourth simulation
N4 <- rep(NA, nYears) ## Population size
X3 <- rep(NA, nYears) ## Random variable for environmental stochasticity
N4[1] <- 500 ## Initial population size
for(t in 2:nYears) {
  X3[t-1] <- rnorm(n=1, mean=0, sd=sigma)
  N4[t] <- (N4[t-1] + N4[t-1]*r + X3[t-1])*(N4[t-1]>extinctionThreshold)
}


# fifth simulation
N5 <- rep(NA, nYears) ## Population size
X4 <- rep(NA, nYears) ## Random variable for environmental stochasticity
N5[1] <- 500 ## Initial population size
for(t in 2:nYears) {
  X4[t-1] <- rnorm(n=1, mean=0, sd=sigma)
  N5[t] <- (N5[t-1] + N5[t-1]*r + X4[t-1])*(N5[t-1]>extinctionThreshold)
}

# sixth simulation
N6 <- rep(NA, nYears) ## Population size
X5 <- rep(NA, nYears) ## Random variable for environmental stochasticity
N6[1] <- 500 ## Initial population size
for(t in 2:nYears) {
  X5[t-1] <- rnorm(n=1, mean=0, sd=sigma)
  N6[t] <- (N6[t-1] + N6[t-1]*r + X5[t-1])*(N6[t-1]>extinctionThreshold)
}

# seventh simulation

N7 <- rep(NA, nYears) ## Population size
X6 <- rep(NA, nYears) ## Random variable for environmental stochasticity
N7[1] <- 500 ## Initial population size
for(t in 2:nYears) {
  X6[t-1] <- rnorm(n=1, mean=0, sd=sigma)
  N7[t] <- (N7[t-1] + N7[t-1]*r + X6[t-1])*(N7[t-1]>extinctionThreshold)
}


# eighth simulation

N8 <- rep(NA, nYears) ## Population size
X7 <- rep(NA, nYears) ## Random variable for environmental stochasticity
N8[1] <- 500 ## Initial population size
for(t in 2:nYears) {
  X7[t-1] <- rnorm(n=1, mean=0, sd=sigma)
  N8[t] <- (N8[t-1] + N8[t-1]*r + X7[t-1])*(N8[t-1]>extinctionThreshold)
}



# ninth simualtion


N9 <- rep(NA, nYears) ## Population size
X8 <- rep(NA, nYears) ## Random variable for environmental stochasticity
N9[1] <- 500 ## Initial population size
for(t in 2:nYears) {
  X8[t-1] <- rnorm(n=1, mean=0, sd=sigma)
  N9[t] <- (N9[t-1] + N9[t-1]*r + X8[t-1])*(N9[t-1]>extinctionThreshold)
}



# tenth simulation

N10 <- rep(NA, nYears) ## Population size
X9 <- rep(NA, nYears) ## Random variable for environmental stochasticity
N10[1] <- 500 ## Initial population size
for(t in 2:nYears) {
  X9[t-1] <- rnorm(n=1, mean=0, sd=sigma)
  N10[t] <- (N10[t-1] + N10[t-1]*r + X9[t-1])*(N10[t-1]>extinctionThreshold)
}

# make data frame
Year<- 0:30
data<- data.frame(Year,N, N1, N2, N3, N4, N5, N6, N7, N8, N9, N10)

#convert data to long format for plotting
require(tidyr)
data1<-data%>% 
  pivot_longer(c("N","N1", "N2", "N3", "N4", "N5", "N6", "N7", "N8", "N9", "N10") ,names_to= "Simulation", values_to= "Count")

#question 1

q2<-N[16]
q2 # Year 15 is when the population will fall below 20 individuals
#question 4
avg<-(22 + 13 + 12 + 15 + 16 + 17 + 16 + 17 +12 + 12)/10
avg

##plot 1----
require(ggplot2)
p1<-ggplot(data1, aes(x=Year, y=Count, group=Simulation, color=Simulation)) + 
  geom_point() + 
  geom_line() 
p1 + scale_y_continuous(name="Abundance") + 
  scale_x_continuous(breaks = seq(0, 30,5),minor_breaks =seq(0,31,1)) +
  theme_classic()

# exercise two----

b <- 0.3 ## Birth rate
d <- 0.2 ## Mortality rate
Years <- 6
N22 <- rep(NA, Years) ## Empty vector for population size
Bt <- rep(NA, Years) ## Random variable for nBirths
Dt <- rep(NA, Years) ## Random variable for nDeaths
N22[1] <- 300 ## Initial population size
for(t in 2:Years) {
  Bt[t-1] <- rpois(n=1, lambda=N22[t-1]*b)
  Dt[t-1] <- rbinom(n=1, size=N22[t-1], prob=d)
  N22[t] <- N22[t-1] + Bt[t-1] - Dt[t-1]
}

Time<- 0:5 #create time variable
data2<-data.frame(Time, N22) #create data frame

##plot 2----
require(ggplot2)
plot2<-ggplot(data2, aes(x=Time, y=N22)) +
  geom_point() +
  geom_line()
 plot2 + scale_y_continuous(name = "Abundance") + theme_classic()

 # question 2,3,4 in excel