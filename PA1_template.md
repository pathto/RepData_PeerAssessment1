# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data


```r
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.1.3
```

```r
data <- read.csv('activity.csv')
data$interval <- sprintf('%02d:%02d',data$interval%/%100, data$interval%%100)
data$time <- strptime(with(data, paste(date, interval)), '%Y-%m-%d %H:%M')
```


## What is mean total number of steps taken per day?

```r
d <- data[!is.na(data$steps), ]
steps_perday <- sapply(split(d,d$date), function(x) sum(x$steps))
#hist(steps_perday)
qplot(steps_perday, binwidth = 2500)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 


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
df <- data.frame(steps = steps, time = names(steps))
plot(strptime(df$time, format = '%H:%M'), df$steps, type = 'l', xlab = 'time', ylab = 'steps')
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
#plot(strptime(names(steps), format='%H:%M'), steps, type='l', xlab='time')
```

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
names(which.max(steps))
```

```
## [1] "08:35"
```

```r
df$time[which.max(df$steps)]
```

```
## [1] 08:35
## 288 Levels: 00:00 00:05 00:10 00:15 00:20 00:25 00:30 00:35 00:40 ... 23:55
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

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png) 

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

```r
data_new$weekday <- weekdays(data_new$time) == 'Sunday' | weekdays(data_new$time) == 'Saturday'
data_new$weekday <- factor(data_new$weekday, label=c('Weekday', 'Weekend'))
d_weekend <- data_new[data_new$weekday=='Weekend',]
d_weekday<- data_new[data_new$weekday=='Weekday',]
steps_weekend <- sapply(split(d_weekend, d_weekend$interval), function(x) mean(x$step))
df_weekend <- data.frame(steps = steps_weekend, time = names(steps_weekend), Weekday = 0)
steps_weekday <- sapply(split(d_weekday, d_weekday$interval), function(x) mean(x$step))
df_weekday <- data.frame(steps = steps_weekday, time = names(steps_weekday), Weekday = 1)

df_new <- rbind(df_weekday, df_weekend)
df_new$Weekday <- factor(df_new$Weekday, labels = c('weekend', 'weekday'))
qplot(strptime(time, format = '%H:%M'), steps, data = df_new, facets = Weekday ~ ., geom = 'line', xlab = 'time')
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 
