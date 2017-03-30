Reproducible Research: Assignment #1
====================================

## Loading and Preprocessing Data

1. Load the data
2. Process/transform the data into a format suitable for analysis


```r
data <-read.csv("activity.csv", header=TRUE, colClasses=c("integer","Date","integer"))
```

## What is the mean total number of steps taken per day?

1. Make a histogram of the total number of steps taken each day
2. Calculate and report the *mean* and *median* total number of steps taken per day


```r
totalSteps <- sapply(split(data$steps, data$date), sum, na.rm=TRUE)
hist(totalSteps)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10-1.png)

```r
meanSteps <- mean(totalSteps)
medianSteps <- median(totalSteps)
```

## What is the average daily activity pattern?

1. Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
avgSteps <- sapply(split(data$steps, data$interval), mean, na.rm=TRUE)
intervals <- unique(data$interval)
plot(x=intervals, y=avgSteps, type="l", xlab="5-min intervals", ylab="avg steps")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png)

```r
maxInterval <- data[which.max(avgSteps),]$interval
```

## Inputing missing values

1. Calculate and report the total number of missing values in the dataset
2. Devise a strategy for filling in all of the missing vlaues in the dataset.  The strategy does not need to be sophisticated.  For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
4. Make a histogram of the total number of steps taken each day and Calculate and report the *mean* and *median* total number of steps taken per day.  Do these values differ from the estimates from the first part of the assignment?  What is the impact of inputing missing data on the estimates of the total daily number of steps?


```r
nrow(na.omit(data))
```

```
## [1] 15264
```

```r
completeData<-data
for (i in 1:nrow(data)){                   # run through our existing data
    if (is.na(data$steps[i])){             # looking for NA steps
       for (n in 1:length(avgSteps)) {     # run though our Average Steps
         if (names(avgSteps)[n] == completeData$interval[i]) { # looking for the same interval value
           completeData$steps[i]<- avgSteps[n]  # replacing our NA with the average steps
         }
       }
    }
}

completeTotSteps <- sapply(split(completeData$steps, completeData$date), sum, na.rm=TRUE)

# plot histogram
hist(completeTotSteps)
```

![plot of chunk unnamed-chunk-12](figure/unnamed-chunk-12-1.png)

```r
# calculate new mean and medium
completeMeanSteps <- mean(completeTotSteps)
completeMedianSteps <- median(completeTotSteps)
```

The mean number of steps changed from **9354** to **10766**, whereas the median changed from **10395** to **10766**.

## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).


```r
library("lattice")

# add column with a factor for the weekend
is.weekend <- function(x) {
  ifelse(weekdays(x) %in% c("Saturday", "Sunday"), TRUE, FALSE)
}

completeData$Weekend <- sapply(completeData$date,is.weekend)
completeData$Weekend <- factor(completeData$Weekend, levels=c(TRUE,FALSE), labels=c("Weekend","Weekday"))

# create two data.frames - one for weekdays and one for weekends
byWorkweek <- split(completeData, completeData$Weekend)

weekend <- byWorkweek$Weekend;
weekday <- byWorkweek$Weekday;

# calculate the steps independently
weekendSteps <- sapply(split(weekend$steps, weekend$interval), sum, na.rm=TRUE)
weekdaySteps <- sapply(split(weekday$steps, weekday$interval), sum, na.rm=TRUE)

# build out new dataframes - steps, interval and day
weekend <- data.frame(steps = weekendSteps, interval = unique(completeData$interval), day="Weekend")
weekday <- data.frame(steps = weekdaySteps, interval = unique(completeData$interval), day="Weekday")

# combine them into one and use the formula steps~interval given 'day' 
xyplot(data=rbind(weekend, weekday) ,steps~interval|day,layout=c(1,2),type="l",ylab="Steps",lwd=2)
```

![plot of chunk unnamed-chunk-13](figure/unnamed-chunk-13-1.png)

In short, **YES!**  Though the number of steps fluctate much more during the week, people are generally more active than on weekends.
