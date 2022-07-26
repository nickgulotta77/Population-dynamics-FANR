require(ggplot2)
require(tidyr)
require(gridExtra)
require(Distance)

#distance sampling analysis----

#data for distance sampling
data<-read.csv("/Users/nickg/OneDrive/Desktop/R projects/Population-dynamics-FANR/data/caribou_data.csv")

#Data manipulation
data2 <- data[,c("Transect", "GroupSize")]
colnames(data2) <- c("Sample.Label", "size") ## Rename
data2$distance <- data$Distance / 1000 ## Convert to km
data2$Region.Label <- 1 ## Just one region
data2$Effort <- data$TransectLength / 1000 
data2$Area <- data$RegionArea ## Square km


model.hn <- ds(data=data2, key="hn",
               transect="line", truncation=250/1000,
               adjustment=NULL, quiet=TRUE)
summary(model.hn)

#plot for detection curve
plot(model.hn, main="Half-normal")


#no development scenario----
#female matrix that accounts for age classes: juveiniles, yearlings, adults
SR <- matrix(c(0,	  0,	    0.019,	0.408,	0.408,	0.408,	0.408,	0.408,	0.408,	0.408,	0.408,	0.408,	0.408,
               0.6,	0,	    0,	    0,	    0,	    0,	    0,      0,      0,      0,      0,      0,      0,
               0,	  0.92,	  0,	    0,	    0,    	0,	    0,      0,      0,      0,      0,      0,      0,          
               0,   0,	    0.950,	0,	    0,	    0,	    0,      0,      0,      0,      0,      0,      0,
               0,	  0,	    0,	    0.83,   0,	    0,	    0,      0,      0,      0,      0,      0,      0,
               0,	  0,      0,	    0,	    0.83,	  0,	    0,      0,      0,      0,      0,      0,      0,
               0,	  0,	    0,	    0,	    0,	    0.83,	  0,      0,      0,      0,      0,      0,      0, ## The first value is s1
               0,	  0,	    0,	    0,	    0,	    0,	  0.83,     0,      0,      0,      0,      0,      0,
               0,	  0,	    0,	    0,	    0,	    0,	  0,        0.83,   0,      0,      0,      0,      0,
               0,	  0,	    0,	    0,	    0,	    0,	  0,        0,   0.83,      0,      0,      0,      0,
               0,	  0,	    0,	    0,	    0,	    0,	  0,        0,      0,      0.83,   0,      0,      0,
               0,	  0,	    0,	    0,	    0,	    0,	  0,        0,      0,      0,      0.83,   0,      0,
               0,	  0,	    0,	    0,	    0,	    0,	  0,        0,      0,      0,      0,      0,      0),
               ## The second value is s2
            nrow=13 ,ncol=13, byrow=TRUE)
require(popbio)
eigen.analysis(SR)

#fecundities
f1<-0#calfs
f2<-0#yearlings
f3<-0.019#2yr
f4<-0.408#3yr
f5<-0.408#4
f6<-0.408#5yr
f7<-0.408#6yr
f8<-0.408#7yr
f9<-0.408#8yr
f10<-0.408#9y
f11<-0.408#10yr
f12<-0.408#11yr
f13<-0.408#12yr

#survival  
s1<-0.6#calf
s2<-0.92#yearlings
s3<-0.95
s4<-0.83
s5<-0.83
s6<-0.83
s7<-0.83
s8<-0.83
s9<-0.83
s10<-0.83
s11<-0.83
s12<-0.83
s13<-0



years1 <- 0:25
nYears1 <- length(years1)
n1 <- rep(NA, nYears1)
n2 <- rep(NA, nYears1)
n3 <- rep(NA, nYears1)
n4 <- rep(NA, nYears1)
n5 <- rep(NA, nYears1)
n6 <- rep(NA, nYears1)
n7 <- rep(NA, nYears1)
n8 <- rep(NA, nYears1)
n9 <- rep(NA, nYears1)
n10 <- rep(NA, nYears1)
n11 <- rep(NA, nYears1)
n12 <- rep(NA, nYears1)
n13 <- rep(NA, nYears1)


#100k tal females (based on 50/50 sex ratio)
n1[1]<-0.222*100000#calf
n2[1]<-0.124*100000#yearlings
n3[1]<-0.115*100000#2yr
n4[1]<-0.10*100000#3yr
n5[1]<-0.091*100000#4yr
n6[1]<-0.075*100000
n7[1]<-0.062*100000
n8[1]<-0.051*100000
n9[1]<-0.042*100000
n10[1]<-0.035*100000
n11[1]<-0.029*100000
n12[1]<-0.024*100000
n13[1]<-0.020*100000



#sim 1----
for(t in 2:nYears1) {
  s1[t-1] <- rnorm(n=1, mean=0.6, sd=0.01)
  n1[t] <- n1[t-1] * f1 + n2[t-1]*f2 + n3[t-1] *f3 + n4[t-1] *f4 + n5[t-1] *f5 + 
    n6[t-1] *f6+ n7[t-1]*f7 + n8[t-1]*f8+ n9[t-1]*f9+ n10[t-1]*f10+ n11[t-1]*f11+ n12[t-1]*f12+ 
    n13[t-1]*f13
  n2[t] <- n1[t-1] * s1 
  n3[t] <- n2[t-1] * s2 
  n4[t] <- n3[t-1] * s3 
  n5[t] <- n4[t-1] * s4 
  n6[t] <- n5[t-1] * s5 
  n7[t] <- n6[t-1] * s6 
  n8[t] <- n7[t-1] * s7 
  n9[t] <- n8[t-1] * s8 
  n10[t]<- n9[t-1] * s9 
  n11[t]<- n10[t-1]* s10 
  n12[t] <- n11[t-1] * s11
  n13[t] <- n12[t-1] * s12
 }

# data frame
dm<-data.frame(years1, n1, n2, n3, n4, n5, n6, n7,n8,n9,n10,n11,n12,n13)



colnames(dm)<-c("Year","Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                "Age_Class_10","Age_Class_11","Age_Class_12" )

#convert data to long format for plotting
require(tidyr)
data6<-dm%>% 
  pivot_longer(c("Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                 "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                 "Age_Class_10","Age_Class_11","Age_Class_12") 
               ,names_to= "Group", values_to= "Abundance")



##plot but shows projection in each age class with no stochasticisty
require(ggplot2)
p1<-ggplot(data6, aes(x=Year, y=Abundance, group=Group, color=Group)) + 
  geom_point() +
  geom_line() +scale_y_continuous("Abundance") + theme_classic() +
  ggtitle(label ="Simulation 1",
          subtitle = "")+
  theme(plot.title = element_text(face = "bold", hjust=0.5), plot.subtitle=element_text(hjust=0.5)) +  # remove legend with theme()
  theme(legend.position = "None")
p1

#sim 2----
for(t in 2:nYears1) {
  s1[t-1] <- rnorm(n=1, mean=0.6, sd=0.01)
  n1[t] <- n1[t-1] * f1 + n2[t-1]*f2 + n3[t-1] *f3 + n4[t-1] *f4 + n5[t-1] *f5 + 
    n6[t-1] *f6+ n7[t-1]*f7 + n8[t-1]*f8+ n9[t-1]*f9+ n10[t-1]*f10+ n11[t-1]*f11+ n12[t-1]*f12+ 
    n13[t-1]*f13
  n2[t] <- n1[t-1] * s1 
  n3[t] <- n2[t-1] * s2 
  n4[t] <- n3[t-1] * s3 
  n5[t] <- n4[t-1] * s4 
  n6[t] <- n5[t-1] * s5 
  n7[t] <- n6[t-1] * s6 
  n8[t] <- n7[t-1] * s7 
  n9[t] <- n8[t-1] * s8 
  n10[t]<- n9[t-1] * s9 
  n11[t]<- n10[t-1]* s10 
  n12[t] <- n11[t-1] * s11
  n13[t] <- n12[t-1] * s12
}

# data frame
dm.2<-data.frame(years1, n1, n2, n3, n4, n5, n6, n7,n8,n9,n10,n11,n12,n13)



colnames(dm.2)<-c("Year","Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                "Age_Class_10","Age_Class_11","Age_Class_12" )

#convert data to long format for plotting
require(tidyr)
data6.2<-dm.2%>% 
  pivot_longer(c("Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                 "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                 "Age_Class_10","Age_Class_11","Age_Class_12") 
               ,names_to= "Group", values_to= "Abundance")



##plot but shows projection in each age class with no stochasticisty
require(ggplot2)
p1.2<-ggplot(data6.2, aes(x=Year, y=Abundance, group=Group, color=Group)) + 
  geom_point() +
  geom_line() +scale_y_continuous("Abundance") + theme_classic() +
  ggtitle(label ="Simulation 2",
          subtitle = "")+
  theme(plot.title = element_text(face = "bold", hjust=0.5), plot.subtitle=element_text(hjust=0.5)) +  # remove legend with theme()
  theme(legend.position = "None")
p1.2

#sim 3----
for(t in 2:nYears1) {
  s1[t-1] <- rnorm(n=1, mean=0.6, sd=0.01)
  n1[t] <- n1[t-1] * f1 + n2[t-1]*f2 + n3[t-1] *f3 + n4[t-1] *f4 + n5[t-1] *f5 + 
    n6[t-1] *f6+ n7[t-1]*f7 + n8[t-1]*f8+ n9[t-1]*f9+ n10[t-1]*f10+ n11[t-1]*f11+ n12[t-1]*f12+ 
    n13[t-1]*f13
  n2[t] <- n1[t-1] * s1 
  n3[t] <- n2[t-1] * s2 
  n4[t] <- n3[t-1] * s3 
  n5[t] <- n4[t-1] * s4 
  n6[t] <- n5[t-1] * s5 
  n7[t] <- n6[t-1] * s6 
  n8[t] <- n7[t-1] * s7 
  n9[t] <- n8[t-1] * s8 
  n10[t]<- n9[t-1] * s9 
  n11[t]<- n10[t-1]* s10 
  n12[t] <- n11[t-1] * s11
  n13[t] <- n12[t-1] * s12
}

# data frame
dm.3<-data.frame(years1, n1, n2, n3, n4, n5, n6, n7,n8,n9,n10,n11,n12,n13)



colnames(dm.3)<-c("Year","Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                "Age_Class_10","Age_Class_11","Age_Class_12" )

#convert data to long format for plotting
require(tidyr)
data6.3<-dm.3%>% 
  pivot_longer(c("Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                 "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                 "Age_Class_10","Age_Class_11","Age_Class_12") 
               ,names_to= "Group", values_to= "Abundance")



##plot but shows projection in each age class with no stochasticisty
require(ggplot2)
p1.3<-ggplot(data6.3, aes(x=Year, y=Abundance, group=Group, color=Group)) + 
  geom_point() +
  geom_line() +scale_y_continuous("Abundance") + theme_classic() +
  ggtitle(label ="Simulation 3",
          subtitle = "")+
  theme(plot.title = element_text(face = "bold", hjust=0.5), plot.subtitle=element_text(hjust=0.5)) 

p1.3

#sim 4----
for(t in 2:nYears1) {
  s1[t-1] <- rnorm(n=1, mean=0.6, sd=0.01)
  n1[t] <- n1[t-1] * f1 + n2[t-1]*f2 + n3[t-1] *f3 + n4[t-1] *f4 + n5[t-1] *f5 + 
    n6[t-1] *f6+ n7[t-1]*f7 + n8[t-1]*f8+ n9[t-1]*f9+ n10[t-1]*f10+ n11[t-1]*f11+ n12[t-1]*f12+ 
    n13[t-1]*f13
  n2[t] <- n1[t-1] * s1 
  n3[t] <- n2[t-1] * s2 
  n4[t] <- n3[t-1] * s3 
  n5[t] <- n4[t-1] * s4 
  n6[t] <- n5[t-1] * s5 
  n7[t] <- n6[t-1] * s6 
  n8[t] <- n7[t-1] * s7 
  n9[t] <- n8[t-1] * s8 
  n10[t]<- n9[t-1] * s9 
  n11[t]<- n10[t-1]* s10 
  n12[t] <- n11[t-1] * s11
  n13[t] <- n12[t-1] * s12
}

# data frame
dm.4<-data.frame(years1, n1, n2, n3, n4, n5, n6, n7,n8,n9,n10,n11,n12,n13)



colnames(dm.4)<-c("Year","Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                  "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                  "Age_Class_10","Age_Class_11","Age_Class_12" )

#convert data to long format for plotting
require(tidyr)
data6.4<-dm.4%>% 
  pivot_longer(c("Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                 "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                 "Age_Class_10","Age_Class_11","Age_Class_12") 
               ,names_to= "Group", values_to= "Abundance")



##plot but shows projection in each age class with no stochasticisty
require(ggplot2)
p1.4<-ggplot(data6.4, aes(x=Year, y=Abundance, group=Group, color=Group)) + 
  geom_point() +
  geom_line() +scale_y_continuous("Abundance") + theme_classic() +
  ggtitle(label ="Simulation 4",
          subtitle = "")+
  theme(plot.title = element_text(face = "bold", hjust=0.5), plot.subtitle=element_text(hjust=0.5)) 

p1.4

#sim 5----
for(t in 2:nYears1) {
  s1[t-1] <- rnorm(n=1, mean=0.6, sd=0.01)
  n1[t] <- n1[t-1] * f1 + n2[t-1]*f2 + n3[t-1] *f3 + n4[t-1] *f4 + n5[t-1] *f5 + 
    n6[t-1] *f6+ n7[t-1]*f7 + n8[t-1]*f8+ n9[t-1]*f9+ n10[t-1]*f10+ n11[t-1]*f11+ n12[t-1]*f12+ 
    n13[t-1]*f13
  n2[t] <- n1[t-1] * s1 
  n3[t] <- n2[t-1] * s2 
  n4[t] <- n3[t-1] * s3 
  n5[t] <- n4[t-1] * s4 
  n6[t] <- n5[t-1] * s5 
  n7[t] <- n6[t-1] * s6 
  n8[t] <- n7[t-1] * s7 
  n9[t] <- n8[t-1] * s8 
  n10[t]<- n9[t-1] * s9 
  n11[t]<- n10[t-1]* s10 
  n12[t] <- n11[t-1] * s11
  n13[t] <- n12[t-1] * s12
}

# data frame
dm.5<-data.frame(years1, n1, n2, n3, n4, n5, n6, n7,n8,n9,n10,n11,n12,n13)



colnames(dm.5)<-c("Year","Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                  "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                  "Age_Class_10","Age_Class_11","Age_Class_12" )

#convert data to long format for plotting
require(tidyr)
data6.5<-dm.5%>% 
  pivot_longer(c("Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                 "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                 "Age_Class_10","Age_Class_11","Age_Class_12") 
               ,names_to= "Group", values_to= "Abundance")



##plot but shows projection in each age class with no stochasticisty
require(ggplot2)
p1.5<-ggplot(data6.5, aes(x=Year, y=Abundance, group=Group, color=Group)) + 
  geom_point() +
  geom_line() +scale_y_continuous("Abundance") + theme_classic() +
  ggtitle(label ="Simulation 5",
          subtitle = "")+
  theme(plot.title = element_text(face = "bold", hjust=0.5), plot.subtitle=element_text(hjust=0.5)) 

p1.5

#create function to get legend extracted for plot
library(gridExtra)
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend<-get_legend(p1.3)

p1.3<-p1.3+theme(legend.position = "none")
p1.4<-p1.4+theme(legend.position = "none")
p1.5<-p1.5+theme(legend.position = "none")
require(gridExtra)
#plot 1----
g1<-grid.arrange(p1, p1.2,p1.3,p1.4, p1.5,legend, ncol=6)

#5 % reduction in calf survival----
SR1 <- matrix(c(0,	  0,	    0.019,	0.408,	0.408,	0.408,	0.408,	0.408,	0.408,	0.408,	0.408,	0.408,	0.408,
               0.57,	0,	    0,	    0,	    0,	    0,	    0,      0,      0,      0,      0,      0,      0,
               0,	  0.92,	  0,	    0,	    0,    	0,	    0,      0,      0,      0,      0,      0,      0,          
               0,   0,	    0.950,	0,	    0,	    0,	    0,      0,      0,      0,      0,      0,      0,
               0,	  0,	    0,	    0.83,   0,	    0,	    0,      0,      0,      0,      0,      0,      0,
               0,	  0,      0,	    0,	    0.83,	  0,	    0,      0,      0,      0,      0,      0,      0,
               0,	  0,	    0,	    0,	    0,	    0.83,	  0,      0,      0,      0,      0,      0,      0, ## The first value is s1
               0,	  0,	    0,	    0,	    0,	    0,	  0.83,     0,      0,      0,      0,      0,      0,
               0,	  0,	    0,	    0,	    0,	    0,	  0,        0.83,   0,      0,      0,      0,      0,
               0,	  0,	    0,	    0,	    0,	    0,	  0,        0,   0.83,      0,      0,      0,      0,
               0,	  0,	    0,	    0,	    0,	    0,	  0,        0,      0,      0.83,   0,      0,      0,
               0,	  0,	    0,	    0,	    0,	    0,	  0,        0,      0,      0,      0.83,   0,      0,
               0,	  0,	    0,	    0,	    0,	    0,	  0,        0,      0,      0,      0,      0,      0),
             ## The second value is s2
             nrow=13 ,ncol=13, byrow=TRUE)
#eigen analysis to calculate lambda values and elasticites
eigen.analysis(SR1)

#fecundities
f1.1<-0#calfs
f2.1<-0#yearlings
f3.1<-0.019#1yr
f4.1<-0.408#2yr
f5.1<-0.408#3
f6.1<-0.408#4yr
f7.1<-0.408#5yr
f8.1<-0.408#6yr
f9.1<-0.408#7yr
f10.1<-0.408
f11.1<-0.408
f12.1<-0.408
f13.1<-0.408

#survival  
s1.1<-0.5#calf
s2.1<-0.92#yearlings
s3.1<-0.95
s4.1<-0.83
s5.1<-0.83
s6.1<-0.83
s7.1<-0.83
s8.1<-0.83
s9.1<-0.83
s10.1<-0.83
s11.1<-0.83
s12.1<-0.83
s13.1<-0



years1 <- 0:25
nYears1 <- length(years1)
n1.1 <- rep(NA, nYears1)
n2.1 <- rep(NA, nYears1)
n3.1 <- rep(NA, nYears1)
n4.1 <- rep(NA, nYears1)
n5.1 <- rep(NA, nYears1)
n6.1 <- rep(NA, nYears1)
n7.1 <- rep(NA, nYears1)
n8.1 <- rep(NA, nYears1)
n9.1 <- rep(NA, nYears1)
n10.1 <- rep(NA, nYears1)
n11.1 <- rep(NA, nYears1)
n12.1 <- rep(NA, nYears1)
n13.1 <- rep(NA, nYears1)


#100k total females
n1.1[1]<-0.222*100000#calf
n2.1[1]<-0.124*100000#yearlings
n3.1[1]<-0.115*100000#2yr
n4.1[1]<-0.10*100000#3yr
n5.1[1]<-0.091*100000#4yr
n6.1[1]<-0.075*100000
n7.1[1]<-0.062*100000
n8.1[1]<-0.051*100000
n9.1[1]<-0.042*100000
n10.1[1]<-0.035*100000
n11.1[1]<-0.029*100000
n12.1[1]<-0.024*100000
n13.1[1]<-0.020*100000

#
s1.1 <- rep(NA, nYears1)

#sim 1----
for(t in 2:nYears1) {
  s1.1[t-1] <- rnorm(n=1, mean=0.57, sd=0.01)
  n1.1[t] <- n1.1[t-1] * f1.1 + n2.1[t-1]*f2.1 + n3.1[t-1] *f3.1 + n4.1[t-1] *f4.1 + n5.1[t-1] *f5.1 + 
    n6.1[t-1] *f6.1+ n7.1[t-1]*f7.1 + n8.1[t-1]*f8.1+ n9.1[t-1]*f9.1+ n10.1[t-1]*f10.1+ n11.1[t-1]*f11.1+ n12.1[t-1]*f12.1+ 
    n13.1[t-1]*f13.1
  n2.1[t] <- n1.1[t-1] * s1.1 
  n3.1[t] <- n2.1[t-1] * s2.1 
  n4.1[t] <- n3.1[t-1] * s3.1 
  n5.1[t] <- n4.1[t-1] * s4.1 
  n6.1[t] <- n5.1[t-1] * s5.1 
  n7.1[t] <- n6.1[t-1] * s6.1 
  n8.1[t] <- n7.1[t-1] * s7.1 
  n9.1[t] <- n8.1[t-1] * s8.1 
  n10.1[t]<- n9.1[t-1] * s9.1 
  n11.1[t]<- n10.1[t-1]* s10.1 
  n12.1[t] <- n11.1[t-1] * s11.1
  n13.1[t] <- n12.1[t-1] * s12.1
}

# data frame
dm1<-data.frame(years1, n1.1, n2.1, n3.1, n4.1, n5.1, n6.1, n7.1,n8.1,n9.1,n10.1,n11.1,n12.1,n13.1)



colnames(dm1)<-c("Year","Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                 "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                 "Age_Class_10","Age_Class_11","Age_Class_12" )

#convert data to long format for plotting
require(tidyr)
data22<-dm1%>% 
  pivot_longer(c("Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                 "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                 "Age_Class_10","Age_Class_11","Age_Class_12") 
               ,names_to= "Group", values_to= "Abundance")



##plot but shows projection in each age class with no stochasticisty
require(ggplot2)
p2<-ggplot(data22, aes(x=Year, y=Abundance, group=Group, color=Group)) + 
  geom_point() +
  geom_line() +scale_y_continuous("Abundance") +theme_classic() + 
  ggtitle("Simulation 1")+
  theme(plot.title = element_text(face = "bold", hjust=0.5))
p2

#sim 2----
for(t in 2:nYears1) {
  s1.1[t-1] <- rnorm(n=1, mean=0.57, sd=0.01)
  n1.1[t] <- n1.1[t-1] * f1.1 + n2.1[t-1]*f2.1 + n3.1[t-1] *f3.1 + n4.1[t-1] *f4.1 + n5.1[t-1] *f5.1 + 
    n6.1[t-1] *f6.1+ n7.1[t-1]*f7.1 + n8.1[t-1]*f8.1+ n9.1[t-1]*f9.1+ n10.1[t-1]*f10.1+ n11.1[t-1]*f11.1+ n12.1[t-1]*f12.1+ 
    n13.1[t-1]*f13.1
  n2.1[t] <- n1.1[t-1] * s1.1 
  n3.1[t] <- n2.1[t-1] * s2.1 
  n4.1[t] <- n3.1[t-1] * s3.1 
  n5.1[t] <- n4.1[t-1] * s4.1 
  n6.1[t] <- n5.1[t-1] * s5.1 
  n7.1[t] <- n6.1[t-1] * s6.1 
  n8.1[t] <- n7.1[t-1] * s7.1 
  n9.1[t] <- n8.1[t-1] * s8.1 
  n10.1[t]<- n9.1[t-1] * s9.1 
  n11.1[t]<- n10.1[t-1]* s10.1 
  n12.1[t] <- n11.1[t-1] * s11.1
  n13.1[t] <- n12.1[t-1] * s12.1
}

# data frame
dm1.2<-data.frame(years1, n1.1, n2.1, n3.1, n4.1, n5.1, n6.1, n7.1,n8.1,n9.1,n10.1,n11.1,n12.1,n13.1)



colnames(dm1.2)<-c("Year","Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                 "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                 "Age_Class_10","Age_Class_11","Age_Class_12" )

#convert data to long format for plotting
require(tidyr)
data22.2<-dm1.2%>% 
  pivot_longer(c("Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                 "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                 "Age_Class_10","Age_Class_11","Age_Class_12") 
               ,names_to= "Group", values_to= "Abundance")



##plot but shows projection in each age class with no stochasticisty
require(ggplot2)
p2.2<-ggplot(data22.2, aes(x=Year, y=Abundance, group=Group, color=Group)) + 
  geom_point() +
  geom_line() +scale_y_continuous("Abundance") +theme_classic() + 
  ggtitle("Simulation 2")+
  theme(plot.title = element_text(face = "bold", hjust=0.5))
p2.2

#sim 3----
for(t in 2:nYears1) {
  s1.1[t-1] <- rnorm(n=1, mean=0.57, sd=0.01)
  n1.1[t] <- n1.1[t-1] * f1.1 + n2.1[t-1]*f2.1 + n3.1[t-1] *f3.1 + n4.1[t-1] *f4.1 + n5.1[t-1] *f5.1 + 
    n6.1[t-1] *f6.1+ n7.1[t-1]*f7.1 + n8.1[t-1]*f8.1+ n9.1[t-1]*f9.1+ n10.1[t-1]*f10.1+ n11.1[t-1]*f11.1+ n12.1[t-1]*f12.1+ 
    n13.1[t-1]*f13.1
  n2.1[t] <- n1.1[t-1] * s1.1 
  n3.1[t] <- n2.1[t-1] * s2.1 
  n4.1[t] <- n3.1[t-1] * s3.1 
  n5.1[t] <- n4.1[t-1] * s4.1 
  n6.1[t] <- n5.1[t-1] * s5.1 
  n7.1[t] <- n6.1[t-1] * s6.1 
  n8.1[t] <- n7.1[t-1] * s7.1 
  n9.1[t] <- n8.1[t-1] * s8.1 
  n10.1[t]<- n9.1[t-1] * s9.1 
  n11.1[t]<- n10.1[t-1]* s10.1 
  n12.1[t] <- n11.1[t-1] * s11.1
  n13.1[t] <- n12.1[t-1] * s12.1
}

# data frame
dm1.3<-data.frame(years1, n1.1, n2.1, n3.1, n4.1, n5.1, n6.1, n7.1,n8.1,n9.1,n10.1,n11.1,n12.1,n13.1)



colnames(dm1.3)<-c("Year","Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                 "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                 "Age_Class_10","Age_Class_11","Age_Class_12" )

#convert data to long format for plotting
require(tidyr)
data22.3<-dm1.3%>% 
  pivot_longer(c("Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                 "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                 "Age_Class_10","Age_Class_11","Age_Class_12") 
               ,names_to= "Group", values_to= "Abundance")



##plot but shows projection in each age class with no stochasticisty
require(ggplot2)
p2.3<-ggplot(data22.3, aes(x=Year, y=Abundance, group=Group, color=Group)) + 
  geom_point() +
  geom_line() +scale_y_continuous("Abundance") +theme_classic() + 
  ggtitle("Simulation 3")+
  theme(plot.title = element_text(face = "bold", hjust=0.5))
p2.3

#sim 4----
for(t in 2:nYears1) {
  s1.1[t-1] <- rnorm(n=1, mean=0.57, sd=0.01)
  n1.1[t] <- n1.1[t-1] * f1.1 + n2.1[t-1]*f2.1 + n3.1[t-1] *f3.1 + n4.1[t-1] *f4.1 + n5.1[t-1] *f5.1 + 
    n6.1[t-1] *f6.1+ n7.1[t-1]*f7.1 + n8.1[t-1]*f8.1+ n9.1[t-1]*f9.1+ n10.1[t-1]*f10.1+ n11.1[t-1]*f11.1+ n12.1[t-1]*f12.1+ 
    n13.1[t-1]*f13.1
  n2.1[t] <- n1.1[t-1] * s1.1 
  n3.1[t] <- n2.1[t-1] * s2.1 
  n4.1[t] <- n3.1[t-1] * s3.1 
  n5.1[t] <- n4.1[t-1] * s4.1 
  n6.1[t] <- n5.1[t-1] * s5.1 
  n7.1[t] <- n6.1[t-1] * s6.1 
  n8.1[t] <- n7.1[t-1] * s7.1 
  n9.1[t] <- n8.1[t-1] * s8.1 
  n10.1[t]<- n9.1[t-1] * s9.1 
  n11.1[t]<- n10.1[t-1]* s10.1 
  n12.1[t] <- n11.1[t-1] * s11.1
  n13.1[t] <- n12.1[t-1] * s12.1
}

# data frame
dm1.4<-data.frame(years1, n1.1, n2.1, n3.1, n4.1, n5.1, n6.1, n7.1,n8.1,n9.1,n10.1,n11.1,n12.1,n13.1)



colnames(dm1.4)<-c("Year","Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                   "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                   "Age_Class_10","Age_Class_11","Age_Class_12" )

#convert data to long format for plotting
require(tidyr)
data22.4<-dm1.4%>% 
  pivot_longer(c("Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                 "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                 "Age_Class_10","Age_Class_11","Age_Class_12") 
               ,names_to= "Group", values_to= "Abundance")



##plot but shows projection in each age class with no stochasticisty
require(ggplot2)
p2.4<-ggplot(data22.4, aes(x=Year, y=Abundance, group=Group, color=Group)) + 
  geom_point() +
  geom_line() +scale_y_continuous("Abundance") +theme_classic() + 
  ggtitle("Simulation 4")+
  theme(plot.title = element_text(face = "bold", hjust=0.5))
p2.4

#sim 5----
for(t in 2:nYears1) {
  s1.1[t-1] <- rnorm(n=1, mean=0.57, sd=0.01)
  n1.1[t] <- n1.1[t-1] * f1.1 + n2.1[t-1]*f2.1 + n3.1[t-1] *f3.1 + n4.1[t-1] *f4.1 + n5.1[t-1] *f5.1 + 
    n6.1[t-1] *f6.1+ n7.1[t-1]*f7.1 + n8.1[t-1]*f8.1+ n9.1[t-1]*f9.1+ n10.1[t-1]*f10.1+ n11.1[t-1]*f11.1+ n12.1[t-1]*f12.1+ 
    n13.1[t-1]*f13.1
  n2.1[t] <- n1.1[t-1] * s1.1 
  n3.1[t] <- n2.1[t-1] * s2.1 
  n4.1[t] <- n3.1[t-1] * s3.1 
  n5.1[t] <- n4.1[t-1] * s4.1 
  n6.1[t] <- n5.1[t-1] * s5.1 
  n7.1[t] <- n6.1[t-1] * s6.1 
  n8.1[t] <- n7.1[t-1] * s7.1 
  n9.1[t] <- n8.1[t-1] * s8.1 
  n10.1[t]<- n9.1[t-1] * s9.1 
  n11.1[t]<- n10.1[t-1]* s10.1 
  n12.1[t] <- n11.1[t-1] * s11.1
  n13.1[t] <- n12.1[t-1] * s12.1
}

# data frame
dm1.5<-data.frame(years1, n1.1, n2.1, n3.1, n4.1, n5.1, n6.1, n7.1,n8.1,n9.1,n10.1,n11.1,n12.1,n13.1)



colnames(dm1.5)<-c("Year","Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                   "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                   "Age_Class_10","Age_Class_11","Age_Class_12" )

#convert data to long format for plotting
require(tidyr)
data22.5<-dm1.5%>% 
  pivot_longer(c("Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                 "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                 "Age_Class_10","Age_Class_11","Age_Class_12") 
               ,names_to= "Group", values_to= "Abundance")



##plot but shows projection in each age class with no stochasticisty
require(ggplot2)
p2.5<-ggplot(data22.5, aes(x=Year, y=Abundance, group=Group, color=Group)) + 
  geom_point() +
  geom_line() +scale_y_continuous("Abundance") +theme_classic() + 
  ggtitle("Simulation 5")+
  theme(plot.title = element_text(face = "bold", hjust=0.5))
p2.5


library(gridExtra)
#function to get the legend for combining plots
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend<-get_legend(p2.3)

#remove legends

p2<-p2+theme(legend.position = "none")
p2.2<-p2.2+theme(legend.position = "none")
p2.3<-p2.3+theme(legend.position = "none")
p2.4<-p2.4+theme(legend.position = "none")
p2.5<-p2.5+theme(legend.position = "none")
#plot simulations into 1 plot
require(gridExtra)
#plot 2----
g2<-grid.arrange(p2, p2.2,p2.3,p2.4, p2.5,legend, ncol=6)


#10% reduction in calf survival---- 
SR2 <- matrix(c(0,	  0,	    0.019,	0.408,	0.408,	0.408,	0.408,	0.408,	0.408,	0.408,	0.408,	0.408,	0.408,
                0.54,	0,	    0,	    0,	    0,	    0,	    0,      0,      0,      0,      0,      0,      0,
                0,	  0.92,	  0,	    0,	    0,    	0,	    0,      0,      0,      0,      0,      0,      0,          
                0,   0,	    0.950,	0,	    0,	    0,	    0,      0,      0,      0,      0,      0,      0,
                0,	  0,	    0,	    0.83,   0,	    0,	    0,      0,      0,      0,      0,      0,      0,
                0,	  0,      0,	    0,	    0.83,	  0,	    0,      0,      0,      0,      0,      0,      0,
                0,	  0,	    0,	    0,	    0,	    0.83,	  0,      0,      0,      0,      0,      0,      0, ## The first value is s1
                0,	  0,	    0,	    0,	    0,	    0,	  0.83,     0,      0,      0,      0,      0,      0,
                0,	  0,	    0,	    0,	    0,	    0,	  0,        0.83,   0,      0,      0,      0,      0,
                0,	  0,	    0,	    0,	    0,	    0,	  0,        0,   0.83,      0,      0,      0,      0,
                0,	  0,	    0,	    0,	    0,	    0,	  0,        0,      0,      0.83,   0,      0,      0,
                0,	  0,	    0,	    0,	    0,	    0,	  0,        0,      0,      0,      0.83,   0,      0,
                0,	  0,	    0,	    0,	    0,	    0,	  0,        0,      0,      0,      0,      0,      0),
              ## The second value is s2
              nrow=13 ,ncol=13, byrow=TRUE)
#eigen analysis to calculate lambda values and elasticities
eigen.analysis(SR2)


#fecundities
f1.11<-0#calfs
f2.11<-0#yearlings
f3.11<-0.019#1yr
f4.11<-0.408#2yr
f5.11<-0.408#3
f6.11<-0.408#4yr
f7.11<-0.408#5yr
f8.11<-0.408#6yr
f9.11<-0.408#7yr
f10.11<-0.408
f11.11<-0.408
f12.11<-0.408
f13.11<-0.408

#survival  
s1.11<-0.5#calf
s2.11<-0.92#yearlings
s3.11<-0.95
s4.11<-0.83
s5.11<-0.83
s6.11<-0.83
s7.11<-0.83
s8.11<-0.83
s9.11<-0.83
s10.11<-0.83
s11.11<-0.83
s12.11<-0.83
s13.11<-0



years1 <- 0:25
nYears1 <- length(years1)
n1.11 <- rep(NA, nYears1)
n2.11 <- rep(NA, nYears1)
n3.11 <- rep(NA, nYears1)
n4.11 <- rep(NA, nYears1)
n5.11 <- rep(NA, nYears1)
n6.11 <- rep(NA, nYears1)
n7.11 <- rep(NA, nYears1)
n8.11 <- rep(NA, nYears1)
n9.11 <- rep(NA, nYears1)
n10.11 <- rep(NA, nYears1)
n11.11 <- rep(NA, nYears1)
n12.11 <- rep(NA, nYears1)
n13.11 <- rep(NA, nYears1)


#100k total females
n1.11[1]<-0.222*100000#calf
n2.11[1]<-0.124*100000#yearlings
n3.11[1]<-0.115*100000#2yr
n4.11[1]<-0.10*100000#3yr
n5.11[1]<-0.091*100000#4yr
n6.11[1]<-0.075*100000
n7.11[1]<-0.062*100000
n8.11[1]<-0.051*100000
n9.11[1]<-0.042*100000
n10.11[1]<-0.035*100000
n11.11[1]<-0.029*100000
n12.11[1]<-0.024*100000
n13.11[1]<-0.020*100000

#
s1.11 <- rep(NA, nYears1)

#sim 1----
for(t in 2:nYears1) {
  s1.11[t-1] <- rnorm(n=1, mean=0.54, sd=0.01)
  n1.11[t] <- n1.11[t-1] * f1.11 + n2.11[t-1]*f2.11 + n3.11[t-1] *f3.11 + n4.11[t-1] *f4.11 + n5.11[t-1] *f5.11 + 
    n6.11[t-1] *f6.11+ n7.11[t-1]*f7.11 + n8.11[t-1]*f8.11+ n9.11[t-1]*f9.11+ n10.11[t-1]*f10.11+ n11.11[t-1]*f11.11+ n12.11[t-1]*f12.11+ 
    n13.11[t-1]*f13.11
  n2.11[t] <- n1.11[t-1] * s1.11 
  n3.11[t] <- n2.11[t-1] * s2.11 
  n4.11[t] <- n3.11[t-1] * s3.11 
  n5.11[t] <- n4.11[t-1] * s4.11 
  n6.11[t] <- n5.11[t-1] * s5.11 
  n7.11[t] <- n6.11[t-1] * s6.11 
  n8.11[t] <- n7.11[t-1] * s7.11 
  n9.11[t] <- n8.11[t-1] * s8.11 
  n10.11[t]<- n9.11[t-1] * s9.11 
  n11.11[t]<- n10.11[t-1]* s10.11 
  n12.11[t] <- n11.11[t-1] * s11.11
  n13.11[t] <- n12.11[t-1] * s12.11
}

# data frame
dm123<-data.frame(years1, n1.11, n2.11, n3.11, n4.11, n5.11, n6.11, n7.11,n8.11,n9.11,n10.11,n11.11,n12.11,n13.11)



colnames(dm123)<-c("Year","Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                 "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                 "Age_Class_10","Age_Class_11","Age_Class_12" )

#convert data to long format for plotting
require(tidyr)
data33<-dm123%>% 
  pivot_longer(c("Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                 "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                 "Age_Class_10","Age_Class_11","Age_Class_12") 
               ,names_to= "Group", values_to= "Abundance")



##plot but shows projection in each age class with no stochasticisty
require(ggplot2)
p3<-ggplot(data33, aes(x=Year, y=Abundance, group=Group, color=Group)) + 
  geom_point() +
  geom_line() +scale_y_continuous("Abundance")+ theme_classic() + 
  ggtitle("Simulation 1")+
  theme(plot.title = element_text(face = "bold", hjust=0.5))
p3

#sim 2----
for(t in 2:nYears1) {
  s1.11[t-1] <- rnorm(n=1, mean=0.54, sd=0.01)
  n1.11[t] <- n1.11[t-1] * f1.11 + n2.11[t-1]*f2.11 + n3.11[t-1] *f3.11 + n4.11[t-1] *f4.11 + n5.11[t-1] *f5.11 + 
    n6.11[t-1] *f6.11+ n7.11[t-1]*f7.11 + n8.11[t-1]*f8.11+ n9.11[t-1]*f9.11+ n10.11[t-1]*f10.11+ n11.11[t-1]*f11.11+ n12.11[t-1]*f12.11+ 
    n13.11[t-1]*f13.11
  n2.11[t] <- n1.11[t-1] * s1.11 
  n3.11[t] <- n2.11[t-1] * s2.11 
  n4.11[t] <- n3.11[t-1] * s3.11 
  n5.11[t] <- n4.11[t-1] * s4.11 
  n6.11[t] <- n5.11[t-1] * s5.11 
  n7.11[t] <- n6.11[t-1] * s6.11 
  n8.11[t] <- n7.11[t-1] * s7.11 
  n9.11[t] <- n8.11[t-1] * s8.11 
  n10.11[t]<- n9.11[t-1] * s9.11 
  n11.11[t]<- n10.11[t-1]* s10.11 
  n12.11[t] <- n11.11[t-1] * s11.11
  n13.11[t] <- n12.11[t-1] * s12.11
}

# data frame
dm123.2<-data.frame(years1, n1.11, n2.11, n3.11, n4.11, n5.11, n6.11, n7.11,n8.11,n9.11,n10.11,n11.11,n12.11,n13.11)



colnames(dm123.2)<-c("Year","Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                   "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                   "Age_Class_10","Age_Class_11","Age_Class_12" )

#convert data to long format for plotting
require(tidyr)
data33.2<-dm123.2%>% 
  pivot_longer(c("Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                 "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                 "Age_Class_10","Age_Class_11","Age_Class_12") 
               ,names_to= "Group", values_to= "Abundance")



##plot but shows projection in each age class with no stochasticisty
require(ggplot2)
p3.2<-ggplot(data33.2, aes(x=Year, y=Abundance, group=Group, color=Group)) + 
  geom_point() +
  geom_line() +scale_y_continuous("Abundance")+ theme_classic() + 
  ggtitle("Simulation 2")+
  theme(plot.title = element_text(face = "bold", hjust=0.5))
p3.2

#sim 3----
for(t in 2:nYears1) {
  s1.11[t-1] <- rnorm(n=1, mean=0.54, sd=0.01)
  n1.11[t] <- n1.11[t-1] * f1.11 + n2.11[t-1]*f2.11 + n3.11[t-1] *f3.11 + n4.11[t-1] *f4.11 + n5.11[t-1] *f5.11 + 
    n6.11[t-1] *f6.11+ n7.11[t-1]*f7.11 + n8.11[t-1]*f8.11+ n9.11[t-1]*f9.11+ n10.11[t-1]*f10.11+ n11.11[t-1]*f11.11+ n12.11[t-1]*f12.11+ 
    n13.11[t-1]*f13.11
  n2.11[t] <- n1.11[t-1] * s1.11 
  n3.11[t] <- n2.11[t-1] * s2.11 
  n4.11[t] <- n3.11[t-1] * s3.11 
  n5.11[t] <- n4.11[t-1] * s4.11 
  n6.11[t] <- n5.11[t-1] * s5.11 
  n7.11[t] <- n6.11[t-1] * s6.11 
  n8.11[t] <- n7.11[t-1] * s7.11 
  n9.11[t] <- n8.11[t-1] * s8.11 
  n10.11[t]<- n9.11[t-1] * s9.11 
  n11.11[t]<- n10.11[t-1]* s10.11 
  n12.11[t] <- n11.11[t-1] * s11.11
  n13.11[t] <- n12.11[t-1] * s12.11
}

# data frame
dm123.3<-data.frame(years1, n1.11, n2.11, n3.11, n4.11, n5.11, n6.11, n7.11,n8.11,n9.11,n10.11,n11.11,n12.11,n13.11)



colnames(dm123.3)<-c("Year","Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                   "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                   "Age_Class_10","Age_Class_11","Age_Class_12" )

#convert data to long format for plotting
require(tidyr)
data33.3<-dm123.3%>% 
  pivot_longer(c("Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                 "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                 "Age_Class_10","Age_Class_11","Age_Class_12") 
               ,names_to= "Group", values_to= "Abundance")



##plot but shows projection in each age class with no stochasticisty
require(ggplot2)
p3.3<-ggplot(data33.3, aes(x=Year, y=Abundance, group=Group, color=Group)) + 
  geom_point() +
  geom_line() +scale_y_continuous("Abundance")+ theme_classic() + 
  ggtitle("Simulation 3")+
  theme(plot.title = element_text(face = "bold", hjust=0.5))
p3.3


#sim 4----
for(t in 2:nYears1) {
  s1.11[t-1] <- rnorm(n=1, mean=0.54, sd=0.01)
  n1.11[t] <- n1.11[t-1] * f1.11 + n2.11[t-1]*f2.11 + n3.11[t-1] *f3.11 + n4.11[t-1] *f4.11 + n5.11[t-1] *f5.11 + 
    n6.11[t-1] *f6.11+ n7.11[t-1]*f7.11 + n8.11[t-1]*f8.11+ n9.11[t-1]*f9.11+ n10.11[t-1]*f10.11+ n11.11[t-1]*f11.11+ n12.11[t-1]*f12.11+ 
    n13.11[t-1]*f13.11
  n2.11[t] <- n1.11[t-1] * s1.11 
  n3.11[t] <- n2.11[t-1] * s2.11 
  n4.11[t] <- n3.11[t-1] * s3.11 
  n5.11[t] <- n4.11[t-1] * s4.11 
  n6.11[t] <- n5.11[t-1] * s5.11 
  n7.11[t] <- n6.11[t-1] * s6.11 
  n8.11[t] <- n7.11[t-1] * s7.11 
  n9.11[t] <- n8.11[t-1] * s8.11 
  n10.11[t]<- n9.11[t-1] * s9.11 
  n11.11[t]<- n10.11[t-1]* s10.11 
  n12.11[t] <- n11.11[t-1] * s11.11
  n13.11[t] <- n12.11[t-1] * s12.11
}

# data frame
dm123.4<-data.frame(years1, n1.11, n2.11, n3.11, n4.11, n5.11, n6.11, n7.11,n8.11,n9.11,n10.11,n11.11,n12.11,n13.11)



colnames(dm123.4)<-c("Year","Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                     "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                     "Age_Class_10","Age_Class_11","Age_Class_12" )

#convert data to long format for plotting
require(tidyr)
data33.4<-dm123.4%>% 
  pivot_longer(c("Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                 "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                 "Age_Class_10","Age_Class_11","Age_Class_12") 
               ,names_to= "Group", values_to= "Abundance")



##plot but shows projection in each age class with no stochasticisty
require(ggplot2)
p3.4<-ggplot(data33.4, aes(x=Year, y=Abundance, group=Group, color=Group)) + 
  geom_point() +
  geom_line() +scale_y_continuous("Abundance")+ theme_classic() + 
  ggtitle("Simulation 4")+
  theme(plot.title = element_text(face = "bold", hjust=0.5))
p3.4

#sim 5----
for(t in 2:nYears1) {
  s1.11[t-1] <- rnorm(n=1, mean=0.54, sd=0.01)
  n1.11[t] <- n1.11[t-1] * f1.11 + n2.11[t-1]*f2.11 + n3.11[t-1] *f3.11 + n4.11[t-1] *f4.11 + n5.11[t-1] *f5.11 + 
    n6.11[t-1] *f6.11+ n7.11[t-1]*f7.11 + n8.11[t-1]*f8.11+ n9.11[t-1]*f9.11+ n10.11[t-1]*f10.11+ n11.11[t-1]*f11.11+ n12.11[t-1]*f12.11+ 
    n13.11[t-1]*f13.11
  n2.11[t] <- n1.11[t-1] * s1.11 
  n3.11[t] <- n2.11[t-1] * s2.11 
  n4.11[t] <- n3.11[t-1] * s3.11 
  n5.11[t] <- n4.11[t-1] * s4.11 
  n6.11[t] <- n5.11[t-1] * s5.11 
  n7.11[t] <- n6.11[t-1] * s6.11 
  n8.11[t] <- n7.11[t-1] * s7.11 
  n9.11[t] <- n8.11[t-1] * s8.11 
  n10.11[t]<- n9.11[t-1] * s9.11 
  n11.11[t]<- n10.11[t-1]* s10.11 
  n12.11[t] <- n11.11[t-1] * s11.11
  n13.11[t] <- n12.11[t-1] * s12.11
}

# data frame
dm123.5<-data.frame(years1, n1.11, n2.11, n3.11, n4.11, n5.11, n6.11, n7.11,n8.11,n9.11,n10.11,n11.11,n12.11,n13.11)



colnames(dm123.5)<-c("Year","Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                     "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                     "Age_Class_10","Age_Class_11","Age_Class_12" )

#convert data to long format for plotting
require(tidyr)
data33.5<-dm123.5%>% 
  pivot_longer(c("Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                 "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                 "Age_Class_10","Age_Class_11","Age_Class_12") 
               ,names_to= "Group", values_to= "Abundance")



##plot but shows projection in each age class with no stochasticisty
require(ggplot2)
p3.5<-ggplot(data33.5, aes(x=Year, y=Abundance, group=Group, color=Group)) + 
  geom_point() +
  geom_line() +scale_y_continuous("Abundance")+ theme_classic() + 
  ggtitle("Simulation 5")+
  theme(plot.title = element_text(face = "bold", hjust=0.5))
p3.5

library(gridExtra)
#function to get the legend for combining plots
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend<-get_legend(p3.3)

#remove legends

p3<-p3+theme(legend.position = "none")
p3.2<-p3.2+theme(legend.position = "none")
p3.3<-p3.3+theme(legend.position = "none")
p3.4<-p3.5+theme(legend.position = "none")
p3.5<-p3.5+theme(legend.position = "none")

#plot simulations into 1 plot
require(gridExtra)

#plot 3----
g3<-grid.arrange(p3, p3.2,p3.3,p3.4, p3.5,legend, ncol=6)



#20% reduction in calf surival---- 
SR3 <- matrix(c(0,	  0,	    0.019,	0.408,	0.408,	0.408,	0.408,	0.408,	0.408,	0.408,	0.408,	0.408,	0.408,
                0.48,	0,	    0,	    0,	    0,	    0,	    0,      0,      0,      0,      0,      0,      0,
                0,	  0.92,	  0,	    0,	    0,    	0,	    0,      0,      0,      0,      0,      0,      0,          
                0,   0,	    0.950,	0,	    0,	    0,	    0,      0,      0,      0,      0,      0,      0,
                0,	  0,	    0,	    0.83,   0,	    0,	    0,      0,      0,      0,      0,      0,      0,
                0,	  0,      0,	    0,	    0.83,	  0,	    0,      0,      0,      0,      0,      0,      0,
                0,	  0,	    0,	    0,	    0,	    0.83,	  0,      0,      0,      0,      0,      0,      0, ## The first value is s1
                0,	  0,	    0,	    0,	    0,	    0,	  0.83,     0,      0,      0,      0,      0,      0,
                0,	  0,	    0,	    0,	    0,	    0,	  0,        0.83,   0,      0,      0,      0,      0,
                0,	  0,	    0,	    0,	    0,	    0,	  0,        0,   0.83,      0,      0,      0,      0,
                0,	  0,	    0,	    0,	    0,	    0,	  0,        0,      0,      0.83,   0,      0,      0,
                0,	  0,	    0,	    0,	    0,	    0,	  0,        0,      0,      0,      0.83,   0,      0,
                0,	  0,	    0,	    0,	    0,	    0,	  0,        0,      0,      0,      0,      0,      0),
              ## The second value is s2
              nrow=13 ,ncol=13, byrow=TRUE)
#eigen analysis to pull lambda values and elasticities
eigen.analysis(SR3)


#fecundities
f1.111<-0#calfs
f2.111<-0#yearlings
f3.111<-0.019#1yr
f4.111<-0.408#2yr
f5.111<-0.408#3
f6.111<-0.408#4yr
f7.111<-0.408#5yr
f8.111<-0.408#6yr
f9.111<-0.408#7yr
f10.111<-0.408
f11.111<-0.408
f12.111<-0.408
f13.111<-0.408

#survival  
s1.111<-0.5#calf
s2.111<-0.92#yearlings
s3.111<-0.95
s4.111<-0.83
s5.111<-0.83
s6.111<-0.83
s7.111<-0.83
s8.111<-0.83
s9.111<-0.83
s10.111<-0.83
s11.111<-0.83
s12.111<-0.83
s13.111<-0



years1 <- 0:25
nYears1 <- length(years1)
n1.111 <- rep(NA, nYears1)
n2.111 <- rep(NA, nYears1)
n3.111 <- rep(NA, nYears1)
n4.111 <- rep(NA, nYears1)
n5.111 <- rep(NA, nYears1)
n6.111 <- rep(NA, nYears1)
n7.111 <- rep(NA, nYears1)
n8.111 <- rep(NA, nYears1)
n9.111 <- rep(NA, nYears1)
n10.111 <- rep(NA, nYears1)
n11.111 <- rep(NA, nYears1)
n12.111 <- rep(NA, nYears1)
n13.111 <- rep(NA, nYears1)


#100k total females
n1.111[1]<-0.222*100000#calf
n2.111[1]<-0.124*100000#yearlings
n3.111[1]<-0.115*100000#2yr
n4.111[1]<-0.10*100000#3yr
n5.111[1]<-0.091*100000#4yr
n6.111[1]<-0.075*100000
n7.111[1]<-0.062*100000
n8.111[1]<-0.051*100000
n9.111[1]<-0.042*100000
n10.111[1]<-0.035*100000
n11.111[1]<-0.029*100000
n12.111[1]<-0.024*100000
n13.111[1]<-0.020*100000

#
s1.111 <- rep(NA, nYears1)

#sim 1----
for(t in 2:nYears1) {
  s1.111[t-1] <- rnorm(n=1, mean=0.48, sd=0.01)
  n1.111[t] <- n1.111[t-1] * f1.111 + n2.111[t-1]*f2.111 + n3.111[t-1] *f3.111 + n4.111[t-1] *f4.111 + n5.111[t-1] *f5.111 + 
    n6.111[t-1] *f6.111+ n7.111[t-1]*f7.111 + n8.111[t-1]*f8.111+ n9.111[t-1]*f9.111+ n10.111[t-1]*f10.111+ n11.111[t-1]*f11.111+ n12.111[t-1]*f12.111+ 
    n13.111[t-1]*f13.111
  n2.111[t] <- n1.111[t-1] * s1.111 
  n3.111[t] <- n2.111[t-1] * s2.111 
  n4.111[t] <- n3.111[t-1] * s3.111 
  n5.111[t] <- n4.111[t-1] * s4.111 
  n6.111[t] <- n5.111[t-1] * s5.111 
  n7.111[t] <- n6.111[t-1] * s6.111 
  n8.111[t] <- n7.111[t-1] * s7.111 
  n9.111[t] <- n8.111[t-1] * s8.111 
  n10.111[t]<- n9.111[t-1] * s9.111 
  n11.111[t]<- n10.111[t-1]* s10.111 
  n12.111[t] <- n11.111[t-1] * s11.111
  n13.111[t] <- n12.111[t-1] * s12.111
}

# data frame
dm44<-data.frame(years1, n1.111, n2.111, n3.111, n4.111, n5.111, n6.111, n7.111,n8.111,n9.111,n10.111,n11.111,n12.111,n13.111)



colnames(dm44)<-c("Year","Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                  "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                  "Age_Class_10","Age_Class_11","Age_Class_12" )

#convert data to long format for plotting
require(tidyr)
data44<-dm44%>% 
  pivot_longer(c("Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                 "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                 "Age_Class_10","Age_Class_11","Age_Class_12") 
               ,names_to= "Group", values_to= "Abundance")



##plot but shows projection in each age class with no stochasticisty
require(ggplot2)
p4<-ggplot(data44, aes(x=Year, y=Abundance,color=Group)) + 
  geom_point() +
  geom_line() +scale_y_continuous("Abundance") + theme_classic() + ggtitle("Simulation 1")+
  theme(plot.title = element_text(face = "bold", hjust=0.5))
p4


#sim 2----
for(t in 2:nYears1) {
  s1.111[t-1] <- rnorm(n=1, mean=0.48, sd=0.01)
  n1.111[t] <- n1.111[t-1] * f1.111 + n2.111[t-1]*f2.111 + n3.111[t-1] *f3.111 + n4.111[t-1] *f4.111 + n5.111[t-1] *f5.111 + 
    n6.111[t-1] *f6.111+ n7.111[t-1]*f7.111 + n8.111[t-1]*f8.111+ n9.111[t-1]*f9.111+ n10.111[t-1]*f10.111+ n11.111[t-1]*f11.111+ n12.111[t-1]*f12.111+ 
    n13.111[t-1]*f13.111
  n2.111[t] <- n1.111[t-1] * s1.111 
  n3.111[t] <- n2.111[t-1] * s2.111 
  n4.111[t] <- n3.111[t-1] * s3.111 
  n5.111[t] <- n4.111[t-1] * s4.111 
  n6.111[t] <- n5.111[t-1] * s5.111 
  n7.111[t] <- n6.111[t-1] * s6.111 
  n8.111[t] <- n7.111[t-1] * s7.111 
  n9.111[t] <- n8.111[t-1] * s8.111 
  n10.111[t]<- n9.111[t-1] * s9.111 
  n11.111[t]<- n10.111[t-1]* s10.111 
  n12.111[t] <- n11.111[t-1] * s11.111
  n13.111[t] <- n12.111[t-1] * s12.111
}

# data frame
dm44.2<-data.frame(years1, n1.111, n2.111, n3.111, n4.111, n5.111, n6.111, n7.111,n8.111,n9.111,n10.111,n11.111,n12.111,n13.111)



colnames(dm44.2)<-c("Year","Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                  "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                  "Age_Class_10","Age_Class_11","Age_Class_12" )

#convert data to long format for plotting
require(tidyr)
data44.2<-dm44.2%>% 
  pivot_longer(c("Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                 "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                 "Age_Class_10","Age_Class_11","Age_Class_12") 
               ,names_to= "Group", values_to= "Abundance")



##plot but shows projection in each age class with no stochasticisty
require(ggplot2)
p4.2<-ggplot(data44, aes(x=Year, y=Abundance,color=Group)) + 
  geom_point() +
  geom_line() +scale_y_continuous("Abundance") + theme_classic() + ggtitle("Simulation 2")+
  theme(plot.title = element_text(face = "bold", hjust=0.5))
p4.2


#sim 3----
for(t in 2:nYears1) {
  s1.111[t-1] <- rnorm(n=1, mean=0.48, sd=0.01)
  n1.111[t] <- n1.111[t-1] * f1.111 + n2.111[t-1]*f2.111 + n3.111[t-1] *f3.111 + n4.111[t-1] *f4.111 + n5.111[t-1] *f5.111 + 
    n6.111[t-1] *f6.111+ n7.111[t-1]*f7.111 + n8.111[t-1]*f8.111+ n9.111[t-1]*f9.111+ n10.111[t-1]*f10.111+ n11.111[t-1]*f11.111+ n12.111[t-1]*f12.111+ 
    n13.111[t-1]*f13.111
  n2.111[t] <- n1.111[t-1] * s1.111 
  n3.111[t] <- n2.111[t-1] * s2.111 
  n4.111[t] <- n3.111[t-1] * s3.111 
  n5.111[t] <- n4.111[t-1] * s4.111 
  n6.111[t] <- n5.111[t-1] * s5.111 
  n7.111[t] <- n6.111[t-1] * s6.111 
  n8.111[t] <- n7.111[t-1] * s7.111 
  n9.111[t] <- n8.111[t-1] * s8.111 
  n10.111[t]<- n9.111[t-1] * s9.111 
  n11.111[t]<- n10.111[t-1]* s10.111 
  n12.111[t] <- n11.111[t-1] * s11.111
  n13.111[t] <- n12.111[t-1] * s12.111
}

# data frame
dm44.3<-data.frame(years1, n1.111, n2.111, n3.111, n4.111, n5.111, n6.111, n7.111,n8.111,n9.111,n10.111,n11.111,n12.111,n13.111)



colnames(dm44.3)<-c("Year","Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                  "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                  "Age_Class_10","Age_Class_11","Age_Class_12" )

#convert data to long format for plotting
require(tidyr)
data44.3<-dm44.3%>% 
  pivot_longer(c("Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                 "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                 "Age_Class_10","Age_Class_11","Age_Class_12") 
               ,names_to= "Group", values_to= "Abundance")



##plot but shows projection in each age class with no stochasticisty
require(ggplot2)
p4.3<-ggplot(data44.3, aes(x=Year, y=Abundance,color=Group)) + 
  geom_point() +
  geom_line() +scale_y_continuous("Abundance") + theme_classic() + ggtitle("Simulation 3")+
  theme(plot.title = element_text(face = "bold", hjust=0.5))
p4.3

#sim 4----
for(t in 2:nYears1) {
  s1.111[t-1] <- rnorm(n=1, mean=0.48, sd=0.01)
  n1.111[t] <- n1.111[t-1] * f1.111 + n2.111[t-1]*f2.111 + n3.111[t-1] *f3.111 + n4.111[t-1] *f4.111 + n5.111[t-1] *f5.111 + 
    n6.111[t-1] *f6.111+ n7.111[t-1]*f7.111 + n8.111[t-1]*f8.111+ n9.111[t-1]*f9.111+ n10.111[t-1]*f10.111+ n11.111[t-1]*f11.111+ n12.111[t-1]*f12.111+ 
    n13.111[t-1]*f13.111
  n2.111[t] <- n1.111[t-1] * s1.111 
  n3.111[t] <- n2.111[t-1] * s2.111 
  n4.111[t] <- n3.111[t-1] * s3.111 
  n5.111[t] <- n4.111[t-1] * s4.111 
  n6.111[t] <- n5.111[t-1] * s5.111 
  n7.111[t] <- n6.111[t-1] * s6.111 
  n8.111[t] <- n7.111[t-1] * s7.111 
  n9.111[t] <- n8.111[t-1] * s8.111 
  n10.111[t]<- n9.111[t-1] * s9.111 
  n11.111[t]<- n10.111[t-1]* s10.111 
  n12.111[t] <- n11.111[t-1] * s11.111
  n13.111[t] <- n12.111[t-1] * s12.111
}

# data frame
dm44.4<-data.frame(years1, n1.111, n2.111, n3.111, n4.111, n5.111, n6.111, n7.111,n8.111,n9.111,n10.111,n11.111,n12.111,n13.111)



colnames(dm44.4)<-c("Year","Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                    "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                    "Age_Class_10","Age_Class_11","Age_Class_12" )

#convert data to long format for plotting
require(tidyr)
data44.4<-dm44.4%>% 
  pivot_longer(c("Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                 "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                 "Age_Class_10","Age_Class_11","Age_Class_12") 
               ,names_to= "Group", values_to= "Abundance")



##plot but shows projection in each age class with no stochasticisty
require(ggplot2)
p4.4<-ggplot(data44.4, aes(x=Year, y=Abundance,color=Group)) + 
  geom_point() +
  geom_line() +scale_y_continuous("Abundance") + theme_classic() + ggtitle("Simulation 4")+
  theme(plot.title = element_text(face = "bold", hjust=0.5))
p4.4

#sim 5----
for(t in 2:nYears1) {
  s1.111[t-1] <- rnorm(n=1, mean=0.48, sd=0.01)
  n1.111[t] <- n1.111[t-1] * f1.111 + n2.111[t-1]*f2.111 + n3.111[t-1] *f3.111 + n4.111[t-1] *f4.111 + n5.111[t-1] *f5.111 + 
    n6.111[t-1] *f6.111+ n7.111[t-1]*f7.111 + n8.111[t-1]*f8.111+ n9.111[t-1]*f9.111+ n10.111[t-1]*f10.111+ n11.111[t-1]*f11.111+ n12.111[t-1]*f12.111+ 
    n13.111[t-1]*f13.111
  n2.111[t] <- n1.111[t-1] * s1.111 
  n3.111[t] <- n2.111[t-1] * s2.111 
  n4.111[t] <- n3.111[t-1] * s3.111 
  n5.111[t] <- n4.111[t-1] * s4.111 
  n6.111[t] <- n5.111[t-1] * s5.111 
  n7.111[t] <- n6.111[t-1] * s6.111 
  n8.111[t] <- n7.111[t-1] * s7.111 
  n9.111[t] <- n8.111[t-1] * s8.111 
  n10.111[t]<- n9.111[t-1] * s9.111 
  n11.111[t]<- n10.111[t-1]* s10.111 
  n12.111[t] <- n11.111[t-1] * s11.111
  n13.111[t] <- n12.111[t-1] * s12.111
}

# data frame
dm44.5<-data.frame(years1, n1.111, n2.111, n3.111, n4.111, n5.111, n6.111, n7.111,n8.111,n9.111,n10.111,n11.111,n12.111,n13.111)



colnames(dm44.5)<-c("Year","Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                    "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                    "Age_Class_10","Age_Class_11","Age_Class_12" )

#convert data to long format for plotting
require(tidyr)
data44.5<-dm44.5%>% 
  pivot_longer(c("Age_Class_0", "Age_Class_1", "Age_Class_2", "Age_Class_3",
                 "Age_Class_4","Age_Class_5","Age_Class_6","Age_Class_7","Age_Class_8","Age_Class_9",
                 "Age_Class_10","Age_Class_11","Age_Class_12") 
               ,names_to= "Group", values_to= "Abundance")



##plot but shows projection in each age class with no stochasticisty
require(ggplot2)
p4.5<-ggplot(data44.3, aes(x=Year, y=Abundance,color=Group)) + 
  geom_point() +
  geom_line() +scale_y_continuous("Abundance") + theme_classic() + ggtitle("Simulation 5")+
  theme(plot.title = element_text(face = "bold", hjust=0.5))
p4.5

library(gridExtra)
#function to get the legend for combining plots
get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}
legend<-get_legend(p4.3)

#remove legends

p4<-p4+theme(legend.position = "none")
p4.2<-p4.2+theme(legend.position = "none")
p4.3<-p4.3+theme(legend.position = "none")
p4.4<-p4.4+theme(legend.position = "none")
p4.5<-p4.5+theme(legend.position = "none")
#plot simulations into 1 plot
require(gridExtra)
#plot 4----
g4<-grid.arrange(p4, p4.2,p4.3,p4.4, p4.5,legend, ncol=6)




