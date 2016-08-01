# PA1_template
Jose Maria Rivas Carvalho  
31 de julio de 2016  

## Loading and preprocessing the data

1.Load the data (i.e. read.csv())

```r
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
file <- "activity.zip"
download.file(url, file)
unzip(file)
```

2.Process/transform the data (if necessary) into a format suitable for your analysis

```r
act <- read.csv("activity.csv", header=TRUE)
head(act)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```


## What is mean total number of steps taken per day?

1.Make a histogram of the total number of steps taken each day

```r
sumact <- with(act, tapply(steps, date, sum, na.rm=TRUE))
dfsumact <- data.frame(dates=names(sumact), total=sumact)
hist(dfsumact$total, main="Histogram of Days", xlab="number of steps", ylab="days", col="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

2.Calculate and report the mean and median total number of steps taken per day

```r
mean(dfsumact$total)
```

```
## [1] 9354.23
```

```r
median(dfsumact$total)
```

```
## [1] 10395
```


## What is the average daily activity pattern?

1.Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
avact <- aggregate(x=list(steps=act$steps), by=list(interval=act$interval), FUN=mean, na.rm=TRUE)
library(ggplot2)
graph <- ggplot(data=avact, aes(x=interval, y=steps))
graph + geom_line() + labs(x="Interval", y="Average Steps", title="Average Steps per Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

2.Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
avact[which.max(avact$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```


## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
miss <- is.na(act$steps)
table(miss)
```

```
## miss
## FALSE  TRUE 
## 15264  2304
```

2.Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

My strategy is to use the mean for the 5-minute interval to fill each NA value in the steps column.

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
fillact <- act 
for (i in 1:nrow(fillact)) {
    if (is.na(fillact$steps[i])) {
        fillact$steps[i] <- avact[which(fillact$interval[i] == avact$interval),]$steps
    }
}
newmiss <- is.na(fillact$steps)
table(newmiss)
```

```
## newmiss
## FALSE 
## 17568
```

4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
sumfillact <- with(fillact, tapply(steps, date, sum, na.rm=TRUE))
dfsumfillact <- data.frame(dates=names(sumfillact), total=sumfillact)
hist(dfsumfillact$total, main="Histogram of Days", xlab="number of steps", ylab="days", col="blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
mean(dfsumfillact$total)
```

```
## [1] 10766.19
```

```r
median(dfsumfillact$total)
```

```
## [1] 10766.19
```


## Are there differences in activity patterns between weekdays and weekends?

1.Create a new factor variable in the dataset with two levels -- "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.

```r
act$day <- weekdays(as.Date(act$date))
day <- c("lunes","martes","miércoles","jueves","viernes","sábado","domingo")
daytype <- c("weekday","weekday","weekday","weekday","weekday","weekend","weekend")
df <- data.frame(day, daytype)
actwd <- merge(act, df, by="day")
head(actwd)
```

```
##       day steps       date interval daytype
## 1 domingo    35 2012-10-21     2040 weekend
## 2 domingo    12 2012-10-21     2000 weekend
## 3 domingo    20 2012-10-21     2035 weekend
## 4 domingo     0 2012-10-21     2240 weekend
## 5 domingo     0 2012-10-21     2245 weekend
## 6 domingo   219 2012-10-21     2005 weekend
```

2.Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was created using simulated data:

```r
avactwd <- aggregate(x=list(steps=actwd$steps), by=list(interval=actwd$interval, daytype=actwd$daytype), FUN=mean, na.rm=TRUE)
graph <- ggplot(data=avactwd, aes(x=interval, y=steps))
graph + geom_line() + facet_grid(daytype~.) + labs(x="Interval", y="Average Steps", title="Average Steps per Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-11-1.png)<!-- -->
