#predicting algorithm
#reload dependencies
library(jsonlite)
library(tidyverse)
library(dplyr)
library(plyr)
library(data.table)
library(ggplot2)
library(scales)
#testdata<-read_json("PUT IN FILE DIRECTORY of TESTING DATA HERE")
#data<-read.csv("PUT IN FILE DIRECTORY of the PROVIDED TRAINING DATA HERE")
data<-read.csv("A://trainingdata.csv")
testdata<-read_json("A://test1.geojson")

#training stage
#first step model: iterative segmented prediction of latitudes
iterator<-function(x){
  l<-list()
  l[[1]]<-lm(nextlat~lat,data=x[x$lat<46.87000,])
  l[[2]]<-lm(nextlat~lat,data=x[x$lat>46.87000,])
  return(l)
}
#store results, split by days
ir<-dlply(data,.(days),iterator)

#Second step model: segmented MLR on longitude split by day

#conduct piece-regression, by day of the week.
bp=c(46.8653,46.8683,46.8750,46.8850) #define breaking points
#define function that returns regressions separated by breaking points.
segmentr<-function(x){
  l<-list()
  l[[1]]<-(lm(long~lat,data=x[x$lat<bp[1],]))
  l[[2]]<-(lm(long~lat,data=x[x$lat>=bp[1]&x$lat<bp[2],]))
  l[[3]]<-(lm(long~lat,data=x[x$lat>=bp[2]&x$lat<bp[3],]))
  l[[4]]<-(lm(long~lat,data=x[x$lat>=bp[3]&x$lat<bp[4],]))
  l[[5]]<-(lm(long~lat,data=x[x$lat>=bp[4],]))
  return(l)
}
#store results, split by days
mlr<-dlply(data,.(days),segmentr)


#prediction (testing) stage
# functions to show day of week
dayrdr<-function(x){
  a<-x[[2]][[1]]$properties$time
  if(as.POSIXlt(a)$wday==1){
    d<-as.POSIXlt(a)$wday+4
  }
  else{
    d<-as.POSIXlt(a)$wday-1
  }
  return(d)
}
dayconvt<-function(x){
  d<-data.frame(dayn=1:5,dayc=c("Tuesday","Wednesday","Thursday","Friday","Monday"))
  a<-d[x,2]
  return(a)
}
#main prediction algorithm

#get the day of the week
day<-dayrdr(testdata)

#get the initial latitude 
latt<-unlist(testdata[[2]][[1]]$geometry$coordinates[[2]])

#get the initial longitude
longt<-unlist(testdata[[2]][[1]]$geometry$coordinates[[1]])

#function that predicts latitude given initial latitude
latgtr<-function(x){
  n<-1
  latvec<-NA
  unc<-list()
  latvec[n]<-predict(ir[[day]][[1]],data.frame(lat=x))
  while (latvec[n]<46.8700){
    latvec[n+1]<-predict(ir[[day]][[1]],data.frame(lat=latvec[n]))
    unc[[n+1]]<-predict(ir[[day]][[1]],data.frame(lat=latvec[n]),interval="prediction")
    n<-n+1
  }
  while (latvec[n]>46.85860){
    latvec[n+1]<-predict(ir[[day]][[2]],data.frame(lat=latvec[n]))
    unc[[n+1]]<-predict(ir[[day]][[1]],data.frame(lat=latvec[n]),interval="prediction")
    n<-n+1
  }
  return(do.call("rbind",unc))
}

predlat<-latgtr(latt) #store predicted latitudes and their intervals 

#function that predicts longitudes given predicted latitudes
longgtr<-function(x,d){
  bp=c(46.8653,46.8683,46.8750,46.8850)
  unc<-list()
  for (i in 1:length(x)){
    if (x[i]<bp[1]){
      unc[[i]]<-predict(mlr[[d]][[1]],data.frame(lat=x[i]),interval="prediction")
    }
    if (x[i]>=bp[1]&x[i]<bp[2]){
      unc[[i]]<-predict(mlr[[d]][[2]],data.frame(lat=x[i]),interval="prediction")
    }
    if (x[i]>=bp[2]&x[i]<bp[3]){
      unc[[i]]<-predict(mlr[[d]][[3]],data.frame(lat=x[i]),interval="prediction")
    }
    if (x[i]>=bp[3]&x[i]<bp[4]){
      unc[[i]]<-predict(mlr[[d]][[4]],data.frame(lat=x[i]),interval="prediction")
    }
    if (x[i]>=bp[4]){
      unc[[i]]<-predict(mlr[[d]][[5]],data.frame(lat=x[i]),interval="prediction")
    }
  }
  return(do.call("rbind",unc))
}
#store predicted longitudes and intervals
predlong<-longgtr(predlat[,1],day)
#form a data frame of predicted values with intervals
predall<-cbind(predlat,predlong)
predall<-as.data.frame(predall)
colnames(predall)<-c("latfit","lwerlat","uprlat","longfit","lwerlong","uprlong")


# KNN function to predict time elapsed since departure, using latitude as a predictor.
knn<- function(xn,k = 5,x,y){
  n <- length(x)
  stopifnot(length(x) == n, length(y) == n,
            length(xn) == 1, k <= n)
  dists <- sqrt((x-xn)^2)
  neighbors <- order(dists)[1:k]
  neighb.out <- y[neighbors]
  return(mean(neighb.out))
}

#fill in time since departure given predicted latitudes
timesincedpt<-NA
for (i in 1:dim(predall)[1]){
  timesincedpt[i]<-knn(predall$latfit[i],x=data$lat,y=data$diffsum)
}
predall<-as.data.frame(cbind(predall,timesincedpt))

#filter first 5 minutes of travel
predall<-predall[predall$timesincedpt>300,]
for (i in 1:dim(predall)[1]-1){
  predall$dist[i]<-as.numeric(rdist.earth(predall[i,c(4,1)],predall[i+1,c(4,1)],miles = T))
  predall$timediff[i]<-abs(predall[i+1,7]-predall[i,7])
}

#filter stationary points, stationary defined as movement less then ~10m in over 2 minutes
for (i in 1:dim(predall)[1]-1){
  if (predall[i+1,]$dist<0.001864&predall[i+1,]$timediff>120){
    predall[i,]<-NULL
  }
}

#prediction function

bomber<-function(x){
  #evaluate uncertainty, locate 2 points with lowest uncertainty
  x$difflat<-x$uprlat-x$lwerlat
  x$difflong<-x$uprlong-x$lwerlong
  x$diffsum<-x$difflat+x$difflong
  a<-head(sort(x$diffsum),2)
  #found. store ideal bomb coordinates
  d<-data.frame(lat=NA,long=NA,time=NA)
  d[1,1]<-x[x$diffsum==a[1],]$latfit
  d[1,2]<-x[x$diffsum==a[1],]$longfit
  d[1,3]<-x[x$diffsum==a[1],]$timesincedpt
  d[2,1]<-x[x$diffsum==a[2],]$latfit
  d[2,2]<-x[x$diffsum==a[2],]$longfit
  d[2,3]<-x[x$diffsum==a[2],]$timesincedpt
  return(d)
}
#get a neat data frame with results
result<-cbind(bomber(predall),rep(dayconvt(day),2))
colnames(result)<-c("predicted latitude","predicted longitude","Time of Explosion","Day of the Week")

#convert time since departure to time of the day
result[1,3]<-as.character(as.POSIXct(testdata$features[[1]]$properties$time_long/1000+as.numeric(result$`Time of Explosion`[1]),origin="1970-01-01",tz="MST"))
result[2,3]<-as.character(as.POSIXct(testdata$features[[1]]$properties$time_long/1000+as.numeric(result$`Time of Explosion`[2]),origin="1970-01-01",tz="MST"))
print(result)


#visualization

p1<-ggplot(predall,aes(x=latfit,y=longfit))+
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10))+
  geom_smooth(data=subset(predall,latfit<46.8653))+
  geom_smooth(data=subset(predall,latfit>=46.8653&latfit<46.8683),col="red")+
  geom_smooth(data=subset(predall,latfit>=46.8683),se=F)+
  geom_smooth(data=subset(predall,latfit<46.8653),aes(x=latfit,y=lwerlong),lty="dashed")+
  geom_smooth(data=subset(predall,latfit>=46.8653&latfit<46.8683),aes(x=latfit,y=lwerlong),col="red",lty="dashed")+
  geom_smooth(data=subset(predall,latfit>=46.8683),aes(x=latfit,y=lwerlong),se=F,lty="dashed")+
  geom_smooth(data=subset(predall,latfit<46.8653),aes(x=latfit,y=uprlong),lty="dashed")+
  geom_smooth(data=subset(predall,latfit>=46.8653&latfit<46.8683),aes(x=latfit,y=uprlong),col="red",lty="dashed")+
  geom_smooth(data=subset(predall,latfit>=46.8683),aes(x=latfit,y=uprlong),se=F,lty="dashed")+
  coord_flip()+
  labs(y="projected latitude",x="projected longitude",title="Projected Path of Target")
  
print(p1)