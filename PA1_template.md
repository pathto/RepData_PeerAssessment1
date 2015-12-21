---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
data <- read.csv('activity.csv')
data$interval <- sprintf('%02d:%02d',data$interval%/%100, data$interval%%100)
data$time <- strptime(with(data, paste(date, interval)), '%Y-%m-%d %H:%M')
```


## What is mean total number of steps taken per day?

```r
d <- data[!is.na(data$steps), ]
steps_perday <- sapply(split(d,d$date), function(x) sum(x$steps))
hist(steps_perday)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 


```r
mean(steps_perday)
```

```
## [1] 9354.23
```

```r
median(steps_perday)
```

```
## [1] 10395
```


## What is the average daily activity pattern?


```r
steps <- sapply(split(d, d$interval), function(x) mean(x$step))
plot(strptime(names(steps), format='%H:%M'), steps, type='l', xlab='time')
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
names(which.max(steps))
```

```
## [1] "08:35"
```

## Imputing missing values
Calculate and report the total number of missing values in the dataset

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

Create a new dataset that is equal to the original dataset but with the missing data filled in, using the mean for that 5-minute interval

```r
nasteps <- is.na(data$steps)
data_new <- data
data_new$steps[nasteps] <- steps[data_new$interval[nasteps]]
```


```r
steps_perday_new <- sapply(split(data_new,data_new$date), function(x) sum(x$steps))
hist(steps_perday_new)
```

![plot of chunk unnamed-chunk-8](figure/unnamed-chunk-8-1.png) 

```r
mean(steps_perday_new)
```

```
## [1] 10766.19
```

```r
median(steps_perday_new)
```

```
## [1] 10766.19
```
These values are greater than the estimates from the first part, 

## Are there differences in activity patterns between weekdays and weekends?
