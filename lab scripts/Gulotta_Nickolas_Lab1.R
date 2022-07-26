
year <- 2000:2020 # A vector of integers
year # Type the name of an object to see its values

nYears <- length(year)
nYears

Juveniles <- rep(NA, nYears) #create empty variable
Juveniles[1] <- 15

#juvenile loop
for(t in 2:12) {
  Juveniles[t] <- Juveniles[t-1] +15
}

for(t in 13:21) {
  Juveniles[t] <- Juveniles[t-1] -15
}

Juveniles




#adult loop

Adults <- rep(NA, nYears)
Adults[1] <- 10


for(t in 2:nYears) {
 Adults[t] <- Juveniles[t-1]* 0.2 + Adults[t-1] * 0.5 
}

Adults








#dataframe
model1 <- data.frame(year, Adults, Juveniles)
model1



#plot
plot(Juveniles ~ year, data=model1, type="o", xlab="Year", ylab="Abundance",
     lwd=2, pch=16, ylim=c(0, 200), col= "purple")
lines(Adults ~ year, data=model1, type="o", col="red", lwd=2, pch=16)
legend(x=2000, y=200, legend=c("Juveniles", "Adults"), col=c("Purple", "Red"), lty=1, pch=16)
