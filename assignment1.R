library(data.table)
library(ggplot2)
library(lubridate)
library(lattice)

dt<-fread("activity.csv")
dt$date<-as.Date(dt$date,"%Y-%m-%d")

#get the sum of steps per day

dt.sum.date = dt[, total_steps:= sum(steps, na.rm = T),by = date]

#histogram - total steps for each date

ggplot(dt.sum.date, aes(x=total_steps)) +
     geom_histogram(colour="black", fill="green") +
     labs(x="Total number of steps per day",y="Frequency",title="Histogram of the total number of steps per day")+
     theme_bw()

#getting the mean and median of total steps per day

mean(dt.sum.date$total_steps)
median(dt.sum.date$total_steps)

#getting the mean across each day

dt.mean.interval = dt[, mean_steps:= mean(steps, na.rm = T),by = interval]

#line graph - Average steps for each interval

ggplot(data=dt.mean.interval, aes(x=interval, y=mean_steps))+
     geom_line(col="blue")+
     labs(x="Interval",y="Mean Steps",title="Mean Steps per Interval")+
     theme_bw()


# get the max mean steps and interval

dt.mean.interval[which.max(dt.mean.interval$mean_steps)]

# draw a vertical line at max

ggplot(data=dt.mean.interval, aes(x=interval, y=mean_steps))+
     geom_line(col="blue")+
     labs(x="Interval",y="Average.Steps",title="Average Steps per Interval")+ 
     geom_vline(xintercept = 835, colour="red", linetype = "longdash")+
     annotate("text", x = 1100, y = 8, label = "max = 835")+
     theme_bw()


#------------Inputing missing data----------------------

#records with/without NA
dt.clean<-dt[complete.cases(dt),]    # sum(is.na(dt)) = 15264
dt.missing<-dt[!complete.cases(dt),]  # sum(!is.na(dt)) = 2304

# total number of NA

nrow(dt[is.na(dt$steps),])

# calculate the mean step per month - for random sampling

 
dt.meansteps.month<-dt[,mean(steps,na.rm=TRUE),by=month(date)]

dt.no.missing<-copy(dt)

set.seed(1)
for(i in 1:nrow(dt)){     
     if(is.na(dt.no.missing$steps[i])){
          dt.no.missing$steps[i]<-sample(30:45,1)
     }
}
          
#get the sum of steps per day - after removing missing values

dt.sum.date.no.missing<-dt.no.missing[, total_steps:= sum(steps), by=date]


#getting the mean and median of total steps per day

mean(dt.sum.date.no.missing$total_steps)
median(dt.sum.date.no.missing$total_steps)

#histogram - total steps for each date - after removing missing values

ggplot(dt.sum.date.no.missing, aes(x=total_steps)) +
     geom_histogram(colour="black", fill="green") +
     labs(x="Total number of steps per day",y="Frequency",title="Histogram of the total number of steps per day")+
     geom_vline(xintercept = mean(dt.sum.date$total_steps), colour="blue", linetype = "longdash")+
     annotate("text", x = 7000, y = 3500, label = "with NA")+
     geom_vline(xintercept = mean(dt.sum.date.no.missing$total_steps), colour="red", linetype = "longdash")+
     annotate("text", x = 14000, y = 3600, label = "without NA")+
     theme_bw() 

# comparing weekdays and weekends

weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
dt.no.missing$dayType = as.factor(ifelse(is.element(weekdays(as.Date(dt.no.missing$date)),weekdays), "Weekday", "Weekend"))

steps_by_interval_i <- aggregate(steps ~ interval + dayType, dt.no.missing, mean)

xyplot(steps_by_interval_i$steps ~ steps_by_interval_i$interval|steps_by_interval_i$dayType, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")

