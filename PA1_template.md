1st Peer Assessment
========================================================
        
        ## Loading and preprocessing the data
        1. Load the data 
2. Process/transform the data (if necessary) into a format suitable for your analysis

```r
wdata<-read.csv("activity.csv", header=TRUE, sep=",", 
                colClass=c("numeric","character","numeric"))
```

```r
wdata$date <- strptime(wdata$date, "%m/%d/%Y")
```

## What is mean total number of steps taken per day?
1. Make a histogram of the total number of steps taken each day
2. Calculate and report the mean and median total number of steps taken per day

```r
gdata<-wdata[!is.na(wdata$steps),]
gdatec<-as.character(gdata$date)
pdata1<-aggregate(x = gdata$steps, by = list(gdatec), FUN = "sum")
colnames(pdata1)<-c("date","steps")
hist(pdata1$steps, xlab="steps per day",col="green", main="Histogram of average steps per day")
```

```
## Error in hist.default(pdata1$steps, xlab = "steps per day", col = "green", : 'x' must be numeric
```

```r
mean(pdata1$steps,na.rm=TRUE)
```

```
## Warning in mean.default(pdata1$steps, na.rm = TRUE): argument is not
## numeric or logical: returning NA
```

```
## [1] NA
```

```r
median(pdata1$steps,na.rm=TRUE)
```

```
## $<NA>
## NULL
```
## What is the average daily activity pattern?
1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
pdata2<-aggregate(x = gdata$steps, by = list(gdata$interval), FUN = "mean")
colnames(pdata2)<-c("interval","steps")
plot(pdata2$interval,pdata2$steps,xlab="interval",ylab="average steps",type="l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

```r
pdata2[which.max(pdata2$steps),1]
```

```
## [1] 835
```
## Imputing missing values
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
sum(is.na(wdata$steps))
```

```
## [1] 2304
```
The method to fill the missing data is to use the mean for the 5-minute interval.

```r
cdata<-wdata
for(i in 1:nrow(wdata)){
        if(is.na(wdata[i,1])){
                cdata[i,1]=pdata2[pdata2$interval==wdata[i,3],2]
        }
}
head(cdata)
```

```
##       steps date interval
## 1 1.7169811 <NA>        0
## 2 0.3396226 <NA>        5
## 3 0.1320755 <NA>       10
## 4 0.1509434 <NA>       15
## 5 0.0754717 <NA>       20
## 6 2.0943396 <NA>       25
```

```r
cdatec<-as.character(cdata$date)
pdata3<-aggregate(x = cdata$steps, by = list(cdatec), FUN = "sum")
colnames(pdata3)<-c("date","steps")
hist(pdata3$steps, xlab="steps per day",col="red",main = "Histogram of average steps per day")
```

```
## Error in hist.default(pdata3$steps, xlab = "steps per day", col = "red", : 'x' must be numeric
```

```r
mean(pdata3$steps,na.rm=TRUE)
```

```
## Warning in mean.default(pdata3$steps, na.rm = TRUE): argument is not
## numeric or logical: returning NA
```

```
## [1] NA
```

```r
median(pdata3$steps,na.rm=TRUE)
```

```
## $<NA>
## NULL
```
From the above result we can see, after imputing the missing data, the median value and the mean value are now the same.
## Are there differences in activity patterns between weekdays and weekends?
1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:
        
        ```r
        cdata$wdays<-weekdays(cdata$date)
        wkend<-c("Saturday","Sunday")
        cdata$whatday<-ifelse(cdata$wdays %in% wkend, "weekend", "weekday")
        cdata$whatday<-factor(cdata$whatday)
        ```

```r
pdata4<-cdata[cdata$whatday=="weekday",]
pdata5<-aggregate(x = pdata4$steps, by = list(pdata4$interval), FUN = "mean")
colnames(pdata5)<-c("interval","steps")
pdata5$whatday = "weekday"
pdata6<-cdata[cdata$whatday=="weekend",]
pdata7<-aggregate(x = pdata6$steps, by = list(pdata6$interval), FUN = "mean")
```

```
## Error in aggregate.data.frame(as.data.frame(x), ...): no rows to aggregate
```

```r
colnames(pdata7)<-c("interval","steps")
```

```
## Error in colnames(pdata7) <- c("interval", "steps"): object 'pdata7' not found
```

```r
pdata7$whatday = "weekend"
```

```
## Error in pdata7$whatday = "weekend": object 'pdata7' not found
```

```r
meanstp<-rbind(pdata5,pdata7)
```

```
## Error in rbind(pdata5, pdata7): object 'pdata7' not found
```

```r
meanstp$whatday<-factor(meanstp$whatday)
```

```
## Error in factor(meanstp$whatday): object 'meanstp' not found
```

```r
library(lattice)

png(filename = "panelplot.png",width = 480, height = 480, units = "px",bg = "transparent")
xyplot(steps ~ interval | whatday, data = meanstp,layout=c(1,2),type="l")
```

```
## Error in eval(substitute(groups), data, environment(x)): object 'meanstp' not found
```

```r
dev.off()
```

```
## RStudioGD 
##         2
```

```r
xyplot(steps ~ interval | whatday, data = meanstp,layout=c(1,2),type="l")
```

```
## Error in eval(substitute(groups), data, environment(x)): object 'meanstp' not found
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)



