install.packages("mra")
require(mra)

capture.histories <- read.table("data/CH-example.inp", sep=" ",
                                colClasses=c("character", "character"),
                                col.names=c("ch", "freq"))
ch.mat <- t(sapply(capture.histories$ch,
                   function(x) as.integer(strsplit(x, "")[[1]])))

dimnames(ch.mat) <- list(paste0("Bear", 1:nrow(capture.histories)),
                         paste0("Time", 1:6))

M0 <- F.huggins.estim(capture=~1, recapture=NULL, histories=ch.mat)
M0

round(M0$p.hat, digits=3)


Mb <- F.huggins.estim(capture=~1, recapture=~1, histories=ch.mat)
Mb

round(Mb$p.hat, digits=3)


time <- tvar(factor(1:6), nan=nrow(ch.mat)) ## 6 time periods. 14 animals.
Mt <- F.huggins.estim(capture=~time, recapture=NULL, histories=ch.mat)
Mt
round(Mt$p.hat, digits=3)


Mtb <- F.huggins.estim(capture=~time, recapture=~1, histories=ch.mat)
Mtb
round(Mtb$p.hat, digits=3)



#Q1

##Q2

#data
capture.histories <- read.table("data/CH-SO-Andy07.inp", sep=" ",
                                colClasses=c("character", "character"),
                                col.names=c("ch", "freq"))
#format data
ch.mat <- t(sapply(capture.histories$ch,
                   function(x) as.integer(strsplit(x, "")[[1]])))

#format data
dimnames(ch.mat) <- list(paste0("StinkPot", 1:nrow(capture.histories)),
                         paste0("Time", 1:6))

#M0 model
M0 <- F.huggins.estim(capture=~1, recapture=NULL, histories=ch.mat)
M0

round(M0$p.hat, digits=3)

#Mb model
Mb <- F.huggins.estim(capture=~1, recapture=~1, histories=ch.mat)
Mb
round(Mb$p.hat, digits=3)
                      
#Mt model                      
time <- tvar(factor(1:6), nan=nrow(ch.mat)) ## 6 time periods. 14 animals.
Mt <- F.huggins.estim(capture=~time, recapture=NULL, histories=ch.mat)
Mt
round(Mt$p.hat, digits=3)

Time<-(c(1, 2, 3, 4, 5, 6))

pt<-(c(0.159, 0.079, 0.185, 0.053 ,0.079,0))

pdata<-data.frame(Time,pt)

require(ggplot2)
ggplot(pdata, aes(x=Time, y=pt)) + geom_point() + geom_line()+
  scale_x_continuous("Time interval", breaks =c(1,2,3,4,5,6)) +
  scale_y_continuous("Capture probability (pt)")+
  theme_classic() +
  ggtitle(label ="Fig 1. The capture probabilty of stinkpots over the season.")+
  theme(plot.title = element_text(face = "bold", hjust=0.5), plot.subtitle=element_text(hjust=0.5))
                      
#Mtb model                    
Mtb <- F.huggins.estim(capture=~time, recapture=~1, histories=ch.mat)
Mtb
round(Mtb$p.hat, digits=3)

