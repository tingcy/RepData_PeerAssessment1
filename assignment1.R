library(data.table)
library(ggplot2)
library(lubridate)
library(lattice)

dt<-fread("activity.csv")
##dt$date<-ymd(dt$date)
dt$date<-as.Date(dt$date,"%Y-%m-%d")

#records with/without NA
dt.clean<-dt[complete.cases(dt),]    # sum(is.na(dt)) = 15264
dt.missing<-dt[!complete.cases(dt),]  # sum(!is.na(dt)) = 2304

#get the mean
dt.clean.mean<-dt.clean[,mean(steps,na.rm=TRUE),by=date]
setnames(dt.clean.mean,"V1","mean")

#get the median
dt.clean.median<-dt.clean[,median(steps,na.rm=TRUE),by=date]
setnames(dt.clean.median,"V1","median")

#get the sum each day
dt.clean.sum<-dt.clean[,sum(steps),by=date]
setnames(dt.clean.sum,"V1","sum")

#bar graph - total steps for each date

ggplot(data=dt.clean.sum, aes(x=date, y=sum)) +
     geom_bar(stat="identity", position="dodge", colour = "white", fill = "steelblue")+
     labs(x="Date",y="Sum",title="Total Steps per Day")+
     theme_bw()

#bar graph - Average steps for each date

ggplot(data=dt.clean.mean, aes(x=date, y=mean,))+
     geom_line(col="blue")+
     labs(x="Date",y="Average",title="Average Steps per Day")+
     theme_bw()

# average steps per interval

dt.clean.average<-dt.clean[,mean(steps),by=interval]
setnames(dt.clean.average,"V1","Average.Steps")

ggplot(data=dt.clean.average, aes(x=interval, y=Average.Steps,))+
     geom_line(col="blue")+
     labs(x="Interval",y="Average Steps",title="Average Steps per Interval")+
     theme_bw()

# get the max average steps and interval
dt.clean.average[which.max(dt.clean.average$Average.Steps)]

# draw a vertical line at max
ggplot(data=dt.clean.average, aes(x=interval, y=Average.Steps,))+
     geom_line(col="blue")+
     labs(x="Interval",y="Average.Steps",title="Average Steps per Interval")+ 
     geom_vline(xintercept = 835, colour="red", linetype = "longdash")+
     annotate("text", x = 1100, y = 8, label = "max = 835")+
     theme_bw()

# total number of NA
nrow(dt.missing<-dt[!complete.cases(dt),] )
nrow(dt[is.na(dt$steps),])

# =================== Inputing missing values ========================

cntTotal<-nrow(dt)
test<-dt[,mean(steps),by=date]
setnames(test,"V1","Means")
test[is.na(test$Means)]<-0   #aggregated means

dt.1<-copy(dt)

# retrive the mean by month--------------------------
dt.1[,mean(steps,na.rm=TRUE),by=month(date)]
------------------------------------------------------
     
     
for(i in 1:cntTotal){     
     if(is.na(dt.1$steps[i])){
          if(is.na(match(dt.1$date[i],test$date))){
               if(month(dt.1$date[i])==10){
                    dt.1$steps[i]<-37.45821
               }else{dt.1$steps[i]<-37.29123}
          }else {dt.1$steps[i]<-test[match(dt.1$date[i],test$date),Means]}          
     }
}

dt.1.mean<-dt.1[,mean(steps),by=date]
setnames(dt.1.mean,"V1","Means")
dt.1.mean$type<-"no.NA"

#---------testing------------------

dt.mean<-dt[,mean(steps),by=date]
setnames(dt.mean,"V1","Means")
dt.mean$type<-"with.NA"

combine.data<-rbind(dt.mean,dt.1.mean)

ggplot(data=combine.data, aes(x=date, y=Means, fill=as.factor(type))) +
     geom_bar(stat="identity", position="dodge")+
     labs(x="Date",y="Sum",title="Total Steps per Day", fill="Type")+
     theme_bw()

ggplot(data=combine.data, aes(x=date, y=Means, group=type, shape=type, colour=type)) +
     geom_line(aes(linetype=type), size=1)+
     theme_bw()

-----------------------------------

#get the median - after filling up missing value
dt.1.median<-dt.1[,median(steps),by=date]
setnames(dt.1.median,"V1","Medians")

ggplot(data=dt.1.mean, aes(x=date, y=Means)) +
     geom_bar(stat="identity", position="dodge", fill="blue")+
     labs(x="Date",y="Mean",title="Average Steps per Day")

# ============= are there differences in activity patterns ===========

weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
dt.1$dayType = as.factor(ifelse(is.element(weekdays(as.Date(dt.1$date)),weekdays), "Weekday", "Weekend"))

steps_by_interval_i <- aggregate(steps ~ interval + dayType, dt.1, mean)
xyplot(steps_by_interval_i$steps ~ steps_by_interval_i$interval|steps_by_interval_i$dayType, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
 
