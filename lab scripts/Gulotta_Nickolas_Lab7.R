# exercise 1----

A <- matrix(c(0, 1, 1, 0.2, ## The fecundities
              0.8, 0.0, 0.0, 0.0, ## The first value is s1
              0.0, 0.5, 0.0, 0.0,
              0.0, 0.0, 0.25, 0), ## The second value is s2
            nrow=4 ,ncol=4, byrow=TRUE)

years1 <- 0:25
nYears1 <- length(years1)
n <- matrix(NA, nYears1, 4)
n[1,] <- c(45, 18, 11, 4) ## Initial abundance

for(t in 2:nYears1) {
  n[t,] <- A %*% n[t-1,] ## Matrix multiplication
}

#sum each row
N <- rowSums(n) ## Total abundance each year
c <- n/N ## This works because of R's recycling rules

#create data frame
data5<-data.frame(years1,c)
colnames(data5)<-c("Year","Age_Class_1", "Age_Class_2", "Age_Class_3", "Age_Class_4")

#convert data to long format for plotting
require(tidyr)
data6<-data5%>% 
  pivot_longer(c("Age_Class_1", "Age_Class_2", "Age_Class_3", "Age_Class_4") 
               ,names_to= "Group", values_to= "Abundance")
##not a required plot but shows proportion in each age class----
require(ggplot2)
p<-ggplot(data6, aes(x=Year, y=Abundance, group=Group, color=Group)) + 
  geom_point() +
  geom_line() +scale_y_continuous("Proportion in each age-class")

p+theme_classic()


#create data frame
data1<-data.frame(years1,n,N)
colnames(data1)<-c("Year","Age_Class_1", "Age_Class_2", "Age_Class_3", "Age_Class_4",
                  "Nt")

#convert data to long format for plotting
require(tidyr)
data2<-data1%>% 
  pivot_longer(c("Age_Class_1", "Age_Class_2", "Age_Class_3", "Age_Class_4",
                 "Nt") ,names_to= "Group", values_to= "Abundance")

##plot 1----
require(ggplot2)
p1<-ggplot(data2, aes(x=Year, y=Abundance, group=Group, color=Group)) + 
  geom_point() +
  geom_line() 

p1+theme_classic()

##log-transform response variable for plot
data2$log<-log(data2$Abundance)
##plot2----
require(ggplot2)
p2<-ggplot(data2, aes(x=Year, y=log, group=Group, color=Group)) + 
  geom_point() +
  geom_line() +
  scale_y_continuous("Abundance (log-transformed)")

p2+theme_classic()

##stable age----
SAD.proj1 <- c[nYears1,]
SAD.proj1

##calculate lambda----
lambda.it <- n[-1,]/n[-nYears1,] ## Divide each row by the row before it
l <- rowSums(lambda.it) ## Total abundance each year
#create data frame
years2<-1:25
data3<-data.frame(years2,lambda.it,l)
colnames(data3)<-c("Year","Age_Class_1", "Age_Class_2", "Age_Class_3", "Age_Class_4",
                   "Nt")

#convert data to long format for plotting
require(tidyr)
data4<-data3%>% 
  pivot_longer(c("Age_Class_1", "Age_Class_2", "Age_Class_3", "Age_Class_4","Nt") ,
               names_to= "Group", values_to= "Lambda")
##plot 3----
require(ggplot2)
p3<-ggplot(data4, aes(x=Year, y=Lambda, group=Group, color=Group)) + 
  geom_point() +
  geom_line() +
  scale_y_continuous("Lambda")

p3+theme_classic()


##lambda----
lambda.it[nYears1-1,] ## These should all be the same

lambda.proj <- lambda.it[nYears1-1,1]
lambda.proj ## Asymptotic growth rate

##asmptotic growth----
eA <- eigen(A)
lambda1 <- Re(eA$values[1])
lambda1 ## Asymptotic growth rate

lambda.proj #same as above

#stable age distribution for each age class
SADu1 <- Re(eA$vectors[,1])
SAD1 <- SADu1/sum(SADu1)
SAD1
SAD.proj1 #same as above

#RRV
eAT <- eigen(t(A))
RVu <- Re(eAT$vectors[,1])
RV <- RVu/RVu[1]
RV



# exercise 2----

b<- matrix(c(1.6,1.5,0.25,0, ## The fecundities
              0.8, 0.0, 0.0, 0.0, ## The first value is s1
              0.0, 0.5, 0.0, 0.0,
              0.0, 0.0, 0.25, 0), ## The second value is s2
            nrow=4 ,ncol=4, byrow=TRUE)

years <- 0:50
nYears <- length(years)
n1 <- matrix(NA, nYears,4)
n2 <- matrix(NA, nYears,4)
n3 <- matrix(NA, nYears,4)
n4 <- matrix(NA, nYears,4)
n1[1,] <- c(200, 0, 0,0) ## Initial abundance
n2[1,] <- c(0, 200, 0,0) ## Initial abundance
n3[1,] <- c(0, 0, 200,0) ## Initial abundance
n4[1,] <- c(0, 0, 0,200) ## Initial abundance

for(t in 2:nYears) {
  n1[t,] <- b %*% n1[t-1,]
  n2[t,] <- b %*% n2[t-1,]
  n3[t,] <- b %*% n3[t-1,]
  n4[t,] <- b %*% n4[t-1,]

  ## Matrix multiplication
}

#create data frame
data<-data.frame(years,n1,n2,n3,n4)
colnames(data)<-c("Year","P1_Age1", "P1_Age2", "P1_Age3", "P1_Age4",
                  "P2_Age1", "P2_Age2", "P2_Age3", "P2_Age4",
                  "P3_Age1", "P3_Age2", "P3_Age3", "P3_Age4",
                  "P4_Age1", "P4_Age2", "P4_Age3", "P4_Age4")
require(dplyr)
df<-data%>%rowwise("P1_Age1", "P1_Age2", "P1_Age3", "P1_Age4")

#compute row sums
d1<-df%>%mutate(P1_Nt=sum(c(P1_Age1, P1_Age2, P1_Age3, P1_Age4)),
                  P2_Nt=sum(c(P2_Age1, P2_Age2, P2_Age3, P2_Age4)),
                  P3_Nt=sum(c(P3_Age1, P3_Age2, P3_Age3, P3_Age4)),
                  P4_Nt=sum(c(P4_Age1, P4_Age2, P4_Age3, P4_Age4)))


##eigen analysis ----
eb <- eigen(b)
lambda <- Re(eb$values[1])
lambda ## Asymptotic growth rate

SADu <- Re(eb$vectors[,1])
SAD <- SADu/sum(SADu)
SAD

##Reproductive values----
ebT <- eigen(t(b))
RVu1 <- Re(ebT$vectors[,1])
RV1 <- RVu1/RVu1[1]
RV1
