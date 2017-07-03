# Reproducible Research: Peer Assessment 1
## Setting working directory

```r
knitr::opts_knit$set(root.dir = 'C:/Users/PWORKLAP/Documents/R-Projects/Reproducible Research/Week 2/')
```
## Loading and preprocessing the data

```r
df <- read.csv("activity.csv")
steps_by_date <- aggregate(.~date, df, sum, na.action = na.omit)
```

## What is mean total number of steps taken per day?

```r
hist(steps_by_date$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mean(steps_by_date$steps)
```

```
## [1] 10766.19
```

```r
median(steps_by_date$steps)
```

```
## [1] 10765
```


## What is the average daily activity pattern?

```r
mean_steps_by_interval <- aggregate(steps~interval, df, mean, na.action = na.omit)
plot(mean_steps_by_interval$interval, mean_steps_by_interval$steps, type = "l", xlab = "5 Minute Interval", ylab = "Average Steps Across All Dates")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
mean_steps_by_interval[which.max(mean_steps_by_interval$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values
1. Calculate and report the total number of missing values in the dataset
(i.e. the total number of rows with NAs)


```r
sum(is.na(df))
```

```
## [1] 2304
```
2. Devise a strategy for filling in all of the missing values in the dataset. The
strategy does not need to be sophisticated. For example, you could use
the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the
missing data filled in.


```r
df2 <- df
for (i in 1:length(df2$steps)){
  if (is.na(df2$steps[i]) == TRUE) {
    df2$steps[i] <- mean_steps_by_interval$steps[mean_steps_by_interval$interval == df2$interval[i]]
  }
}
```

4. Make a histogram of the total number of steps taken each day and Calculate
and report the mean and median total number of steps taken per day. Do
these values differ from the estimates from the first part of the assignment?


```r
steps_by_date2 <- aggregate(.~date, df2, sum, na.action = na.omit)
hist(steps_by_date2$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

```r
mean(steps_by_date2$steps)
```

```
## [1] 10766.19
```

```r
median(steps_by_date2$steps)
```

```
## [1] 10766.19
```

What is the impact of imputing missing data on the estimates of the total
daily number of steps?

Answer:  the mean stays the same, but the median is shifted upward.

## Are there differences in activity patterns between weekdays and weekends?

```r
require(ggplot2)
```

```
## Loading required package: ggplot2
```

```
## Warning: package 'ggplot2' was built under R version 3.3.3
```

```r
df2$day <- weekdays(as.Date(df2$date, format = "%Y-%m-%d"))
for (i in 1:length(df2$steps)) {
  if (df2$day[i] == "Saturday" | df2$day[i] == "Sunday") {
    df2$daytype[i] <- "weekend"
  } else {df2$daytype[i] <- "weekday"}
}
mean_steps_by_daytype <- aggregate(steps~interval+daytype, df2, mean, na.action = na.omit)
ggplot(aes(x = interval, y = steps), data = mean_steps_by_daytype) + geom_line() + facet_wrap(~daytype, ncol = 1) + ylab("Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->
