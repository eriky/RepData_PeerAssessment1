# Reproducible Research: Peer Assessment 1
# Reproducible Research: Peer Assessment 1

## Loading and preprocessing the data
First of all, I force the locale to US english. Then I load the data and convert the dates from factors to actual dates.

```r
Sys.setlocale("LC_TIME", "en_US")
```

```
## [1] "en_US"
```

```r
setwd("~/datascience/5. Reproducible Research/RepData_PeerAssessment1")
data <- read.csv(unz("activity.zip", "activity.csv"))
data$date <- as.Date(data$date)
```

## What is mean total number of steps taken per day?

```r
steps_per_day = aggregate(steps ~ date, data, sum)
hist(steps_per_day$steps, xlab="Steps per day", main="Histogram of steps per day")
```

![plot of chunk unnamed-chunk-2](./PA1_template_files/figure-html/unnamed-chunk-2.png) 

```r
# Mean number of total steps per day
mean_per_day=mean(steps_per_day$steps)
# Median number of total steps per day
median_per_day=median(steps_per_day$steps)
```

The mean total steps per day is 1.0766 &times; 10<sup>4</sup> and the median total number of steps per day is 10765.

## What is the average daily activity pattern?

```r
data$date = NULL
mean_steps_by_interval = aggregate(.~interval, FUN=mean, data=data)

plot(mean_steps_by_interval, type="l")
```

![plot of chunk unnamed-chunk-3](./PA1_template_files/figure-html/unnamed-chunk-3.png) 

```r
# The highest interval
mean_steps_by_interval[which.max(mean_steps_by_interval$steps),]
```

```
##     interval steps
## 104      835 206.2
```

## Imputing missing values

```r
data <- read.csv(unz("activity.zip", "activity.csv"))
data_imputed = data
data_imputed$date <- as.Date(data_imputed$date)

# Number of NA's
sum(is.na(data$steps))
```

```
## [1] 2304
```

### Use the previously calculated means per 5-minute interval to fill empty spots in the dataset

My Strategy is to use the previously computed means per interval. In a loop I will fill out any interval that has a NA in it with the mean of that interval over all other days.


```r
for (i in 1:nrow(data_imputed)) {
  if (is.na(data_imputed[i,]$steps)) {
    data_imputed[i,]$steps = mean_steps_by_interval[which(mean_steps_by_interval$interval==data_imputed[i,]$interval), ]$steps
  }
}

steps_per_day = aggregate(steps ~ date, data_imputed, sum)
hist(steps_per_day$steps, xlab="Steps per day", main="Histogram of steps per day")
```

![plot of chunk unnamed-chunk-5](./PA1_template_files/figure-html/unnamed-chunk-5.png) 

```r
mean(steps_per_day$steps)
```

```
## [1] 10766
```

```r
median(steps_per_day$steps)
```

```
## [1] 10766
```
The mean value does not differ from the non-imputed data, which is not surprising since I added mean values in the missing spots which will not change anything to the overall mean.
The median has changed only slightly.

## Are there differences in activity patterns between weekdays and weekends?

```r
weekends = weekdays(data_imputed$date) %in% c("Saturday", "Sunday")
data_imputed$daytype = factor(weekends, labels=c("weekday", "weekend"))

mean_steps_by_interval_weekends = aggregate(.~interval, FUN=mean, data=data_imputed[which(data_imputed$daytype=="weekend"),])
mean_steps_by_interval_weekdays = aggregate(.~interval, FUN=mean, data=data_imputed[which(data_imputed$daytype=="weekday"),])

mean_steps_by_interval_weekdays$date = NULL
mean_steps_by_interval_weekdays$daytype = NULL
mean_steps_by_interval_weekends$date = NULL
mean_steps_by_interval_weekends$daytype = NULL
par(mfrow=c(2,1))


with(data, { 
  plot(mean_steps_by_interval_weekends, type="l", ylab="Number of steps", main="Mean steps per interval in weekends")
  plot(mean_steps_by_interval_weekdays, type="l", ylab="Number of steps", main="Mean steps per interval on weekdays")
})
```

![plot of chunk unnamed-chunk-6](./PA1_template_files/figure-html/unnamed-chunk-6.png) 


As can be seen in the graph above, there is more walking activity during the day in the weekends than during weekdays. This could be due to a day job where the person mostly sits.
