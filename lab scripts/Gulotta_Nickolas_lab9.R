install.packages("unmarked")
require(unmarked)

require(readr)

#data
quail<-read.csv("data/quail.csv")
dipnet<-read.csv("data/dipnet.csv")
exdata<-read.csv("data/example-data.csv")


## First, extract columns 2-4 with the occupancy data
occData <- quail[,c("season1visit1", "season1visit2",
                          "season1visit3", "season1visit4")]

## Next, extract the column with the site covariate.
## It must be formatted as a data.frame
habIndex <- quail[,"veght",drop=FALSE]
## Finally, put the pieces together
umf <- unmarkedFrameOccu(y=occData, siteCovs=habIndex)

summary(umf)

#null model
nullModel <- occu(~1 ~1, umf)
summary(nullModel)


## Occupancy estimate - Pr(site is occupied)
backTransform(nullModel, type="state")


## Detection estimate - Pr(detection | site is occupied)
backTransform(nullModel, type="det")


#model 2 habitat index
detNullOccHab <- occu(~1 ~veght, umf)
summary(detNullOccHab)

#visualize
predData <- data.frame(veght=seq(from=1, to=5, length.out=10))

predOcc <- predict(detNullOccHab, newdata=predData,
                   type="state", append=TRUE)



#plot
plot(Predicted ~ veght, data=predOcc, type="l", ylim=c(0,1),
     xlab="Habitat index", ylab="Occupancy")
lines(lower ~ veght, data=predOcc, lty=2)
lines(upper ~ veght, data=predOcc, lty=2)




# ex. 2
occData <- dipnet[,c("X2015_TeamA", "X2015_TeamB", "X2015_TeamC", "X2015_TeamD", "X2015_TeamE", "X2015_TeamF",
                     "X2015_TeamG" ,"X2016_TeamA", "X2016_TeamB", "X2016_TeamC", "X2016_TeamD", "X2016_TeamE",
                      "X2016_TeamF" ,"X2016_TeamG", "X2017_TeamA", "X2017_TeamB", "X2017_TeamC", "X2017_TeamD",
                      "X2017_TeamE" ,"X2017_TeamF", "X2017_TeamG", "X2018_TeamA", "X2018_TeamB", "X2018_TeamC",
                      "X2018_TeamD", "X2018_TeamE", "X2018_TeamF", "X2018_TeamG", "X2019_TeamA", "X2019_TeamB",
                     "X2019_TeamC", "X2019_TeamD", "X2019_TeamE", "X2019_TeamF", "X2019_TeamG")]
umfMS <- unmarkedMultFrame(y=occData, numPrimary=5)

summary(umfMS)


#null model
nullModelMS <- colext(~1, ~1, ~1, ~1, umfMS)
summary(nullModelMS)


#backtransform
psi<-backTransform(nullModelMS, type="psi")

install.packages("tidyverse")
require(tidyverse)
require(broom)


psi<-backTransform(nullModelMS, type="col")
psi
ext<-backTransform(nullModelMS, type="ext")
ext
det<-backTransform(nullModelMS, type="psi")
det

Es<-(c(0.98, 0.000139, 0.294))
se<-(c(0.164, 0.00508, 0.0415))

tab<-data.frame(Term,Es,se)

Term<-(c("psi", "gamma", "epsilon"))

