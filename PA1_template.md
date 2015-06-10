# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

The loading process begins with loading all the required libraries:


```r
library(data.table)
library(ggplot2)
library(lubridate)
library(lattice)
```
The next steps is to read the file

```r
dt<-fread("activity.csv")
dt$date<-as.Date(dt$date,"%Y-%m-%d")
```
Records with/without NA

```r
dt.clean<-dt[complete.cases(dt),]    # sum(is.na(dt)) = 15264
dt.missing<-dt[!complete.cases(dt),]  # sum(!is.na(dt)) = 2304
```


Get the sum each day

```r
dt.clean.sum<-dt.clean[,sum(steps),by=date]
setnames(dt.clean.sum,"V1","sum")
```

## What is mean total number of steps taken per day?

Get the mean for clean data

```r
dt.clean.mean<-dt.clean[,mean(steps,na.rm=TRUE),by=date]
setnames(dt.clean.mean,"V1","mean")
dt.clean.mean
```

```
##           date       mean
##  1: 2012-10-02  0.4375000
##  2: 2012-10-03 39.4166667
##  3: 2012-10-04 42.0694444
##  4: 2012-10-05 46.1597222
##  5: 2012-10-06 53.5416667
##  6: 2012-10-07 38.2465278
##  7: 2012-10-09 44.4826389
##  8: 2012-10-10 34.3750000
##  9: 2012-10-11 35.7777778
## 10: 2012-10-12 60.3541667
## 11: 2012-10-13 43.1458333
## 12: 2012-10-14 52.4236111
## 13: 2012-10-15 35.2048611
## 14: 2012-10-16 52.3750000
## 15: 2012-10-17 46.7083333
## 16: 2012-10-18 34.9166667
## 17: 2012-10-19 41.0729167
## 18: 2012-10-20 36.0937500
## 19: 2012-10-21 30.6284722
## 20: 2012-10-22 46.7361111
## 21: 2012-10-23 30.9652778
## 22: 2012-10-24 29.0104167
## 23: 2012-10-25  8.6527778
## 24: 2012-10-26 23.5347222
## 25: 2012-10-27 35.1354167
## 26: 2012-10-28 39.7847222
## 27: 2012-10-29 17.4236111
## 28: 2012-10-30 34.0937500
## 29: 2012-10-31 53.5208333
## 30: 2012-11-02 36.8055556
## 31: 2012-11-03 36.7048611
## 32: 2012-11-05 36.2465278
## 33: 2012-11-06 28.9375000
## 34: 2012-11-07 44.7326389
## 35: 2012-11-08 11.1770833
## 36: 2012-11-11 43.7777778
## 37: 2012-11-12 37.3784722
## 38: 2012-11-13 25.4722222
## 39: 2012-11-15  0.1423611
## 40: 2012-11-16 18.8923611
## 41: 2012-11-17 49.7881944
## 42: 2012-11-18 52.4652778
## 43: 2012-11-19 30.6979167
## 44: 2012-11-20 15.5277778
## 45: 2012-11-21 44.3993056
## 46: 2012-11-22 70.9270833
## 47: 2012-11-23 73.5902778
## 48: 2012-11-24 50.2708333
## 49: 2012-11-25 41.0902778
## 50: 2012-11-26 38.7569444
## 51: 2012-11-27 47.3819444
## 52: 2012-11-28 35.3576389
## 53: 2012-11-29 24.4687500
##           date       mean
```

Get the median for clean data

```r
dt.clean.median<-dt.clean[,median(steps,na.rm=TRUE),by=date]
setnames(dt.clean.median,"V1","median")
dt.clean.median
```

```
##           date median
##  1: 2012-10-02      0
##  2: 2012-10-03      0
##  3: 2012-10-04      0
##  4: 2012-10-05      0
##  5: 2012-10-06      0
##  6: 2012-10-07      0
##  7: 2012-10-09      0
##  8: 2012-10-10      0
##  9: 2012-10-11      0
## 10: 2012-10-12      0
## 11: 2012-10-13      0
## 12: 2012-10-14      0
## 13: 2012-10-15      0
## 14: 2012-10-16      0
## 15: 2012-10-17      0
## 16: 2012-10-18      0
## 17: 2012-10-19      0
## 18: 2012-10-20      0
## 19: 2012-10-21      0
## 20: 2012-10-22      0
## 21: 2012-10-23      0
## 22: 2012-10-24      0
## 23: 2012-10-25      0
## 24: 2012-10-26      0
## 25: 2012-10-27      0
## 26: 2012-10-28      0
## 27: 2012-10-29      0
## 28: 2012-10-30      0
## 29: 2012-10-31      0
## 30: 2012-11-02      0
## 31: 2012-11-03      0
## 32: 2012-11-05      0
## 33: 2012-11-06      0
## 34: 2012-11-07      0
## 35: 2012-11-08      0
## 36: 2012-11-11      0
## 37: 2012-11-12      0
## 38: 2012-11-13      0
## 39: 2012-11-15      0
## 40: 2012-11-16      0
## 41: 2012-11-17      0
## 42: 2012-11-18      0
## 43: 2012-11-19      0
## 44: 2012-11-20      0
## 45: 2012-11-21      0
## 46: 2012-11-22      0
## 47: 2012-11-23      0
## 48: 2012-11-24      0
## 49: 2012-11-25      0
## 50: 2012-11-26      0
## 51: 2012-11-27      0
## 52: 2012-11-28      0
## 53: 2012-11-29      0
##           date median
```

total steps for each date


```r
ggplot(data=dt.clean.sum, aes(x=date, y=sum)) +
     geom_bar(stat="identity", position="dodge", colour = "white", fill ="steelblue")+
     labs(x="Date",y="Sum",title="Total Steps per Day")+
     theme_bw()
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

Average steps for each date


```r
ggplot(data=dt.clean.mean, aes(x=date, y=mean,))+
     geom_line(col="blue")+
     labs(x="Date",y="Average",title="Average Steps per Day")+
     theme_bw()
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

## What is the average daily activity pattern?


```r
dt.clean.average<-dt.clean[,mean(steps),by=interval]
setnames(dt.clean.average,"V1","Average.Steps")
```

```r
ggplot(data=dt.clean.average, aes(x=interval, y=Average.Steps,))+
     geom_line(col="blue")+
     labs(x="Interval",y="Average Steps",title="Average Steps per Interval")+
     theme_bw()
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png) 

Get the max average steps and interval

```r
dt.clean.average[which.max(dt.clean.average$Average.Steps)]
```

```
##    interval Average.Steps
## 1:      835      206.1698
```

Draw a vertical line at max

```r
ggplot(data=dt.clean.average, aes(x=interval, y=Average.Steps,))+
     geom_line(col="blue")+
     labs(x="Interval",y="Average.Steps",title="Average Steps per Interval")+ 
     geom_vline(xintercept = 835, colour="red", linetype = "longdash")+
     annotate("text", x = 1100, y = 8, label = "max = 835")+
     theme_bw()
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 

## Imputing missing values

Total number of NA

```r
nrow(dt.missing<-dt[!complete.cases(dt),] )
```

```
## [1] 2304
```

```r
nrow(dt[is.na(dt$steps),])
```

```
## [1] 2304
```

```r
cntTotal<-nrow(dt)
test<-dt[,mean(steps),by=date]
setnames(test,"V1","Means")
test[is.na(test$Means)]<-0   
dt.1<-copy(dt) 
dt.1[,mean(steps,na.rm=TRUE),by=month(date)] 
```

```
##    month       V1
## 1:    10 37.45821
## 2:    11 37.29123
```

```r
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
 

dt.mean<-dt[,mean(steps),by=date]
setnames(dt.mean,"V1","Means")
dt.mean$type<-"with.NA"

combine.data<-rbind(dt.mean,dt.1.mean)
```

plot graph

```r
ggplot(data=combine.data, aes(x=date, y=Means, fill=as.factor(type))) +
     geom_bar(stat="identity", position="dodge")+
     labs(x="Date",y="Sum",title="Total Steps per Day", fill="Type")+
     theme_bw()
```

![](PA1_template_files/figure-html/unnamed-chunk-14-1.png) 
plot line graph

```r
ggplot(data=combine.data, aes(x=date, y=Means, group=type, shape=type, colour=type)) +
     geom_line(aes(linetype=type), size=1)+
     theme_bw()
```

```
## Warning in loop_apply(n, do.ply): Removed 2 rows containing missing values
## (geom_path).
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png) 

Get the median - after filling up missing value

```r
dt.1.median<-dt.1[,median(steps),by=date]
setnames(dt.1.median,"V1","Medians")
```

Plot bar graph

```r
ggplot(data=dt.1.mean, aes(x=date, y=Means)) +
     geom_bar(stat="identity", position="dodge", fill="blue")+
     labs(x="Date",y="Mean",title="Average Steps per Day")
```

![](PA1_template_files/figure-html/unnamed-chunk-17-1.png) 

## Are there differences in activity patterns between weekdays and weekends?


```r
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
dt.1$dayType = as.factor(ifelse(is.element(weekdays(as.Date(dt.1$date)),weekdays), "Weekday", "Weekend"))
```

Plot xyplot graph

```r
steps_by_interval_i <- aggregate(steps ~ interval + dayType, dt.1, mean)
xyplot(steps_by_interval_i$steps ~ steps_by_interval_i$interval|steps_by_interval_i$dayType, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-19-1.png) 
 

