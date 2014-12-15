# Reproducible Research: Peer Assessment 1

The `dplyr` package will be used to assist with some of the data manipulations.


```r
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following object is masked from 'package:stats':
## 
##     filter
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

## Loading and preprocessing the data

No preprocessing is necessary, simply load the data with `read.csv`.


```r
unzip('activity.zip')
data <- read.csv('activity.csv', colClasses=c('numeric', 'Date', 'numeric'))
```

## What is mean total number of steps taken per day?

In the following analysis, `NA` values will be ignored. The first problem is to make a histogram of the total number of steps taken each day using the following R snippet.


```r
groupedByDay <- group_by(data, date)
stepsPerDay <- summarise(groupedByDay, steps=sum(steps, na.rm=TRUE))
hist(stepsPerDay$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

Next, we can calculate the mean and median number of steps taken per day.


```r
mean(stepsPerDay$steps)
```

```
## [1] 9354.23
```

```r
median(stepsPerDay$steps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?

We can begin this analysis by making a time series plot of the of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days.


```r
groupedByInterval <- group_by(data, interval)
meanStepsPerInterval <- summarise(groupedByInterval, steps=mean(steps, na.rm=TRUE))
plot(meanStepsPerInterval, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

Now we can find which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
meanStepsPerInterval[meanStepsPerInterval$steps == max(meanStepsPerInterval$steps),]
```

```
## Source: local data frame [1 x 2]
## 
##   interval    steps
## 1      835 206.1698
```

## Imputing missing values

To aid our analysis, we can impute missing values in the dataset. First, let's find out how many days have missing data.


```r
sum(is.na(data$steps))
```

```
## [1] 2304
```

To fill in our missing data, we can use the mean value over the entire data set.


```r
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm=TRUE))
imputedData <- data
imputedData$steps <- impute.mean(data$steps)
```

Now we can repeat our previous analysis using our data set with imputed values added. First by plotting a histogram.


```r
groupedByDay <- group_by(imputedData, date)
stepsPerDay <- summarise(groupedByDay, steps=sum(steps, na.rm=TRUE))
hist(stepsPerDay$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png) 

And second by calculating the mean and median.


```r
mean(stepsPerDay$steps)
```

```
## [1] 10766.19
```

```r
median(stepsPerDay$steps)
```

```
## [1] 10766.19
```

You can see that the values are higher. This makes sense because values of NA in our original data set were treated as `0` and in our new data set they were treated as a higher value (the mean).

## Are there differences in activity patterns between weekdays and weekends?

We can find out about differences in activity patterns between weekdays and weekends by creating a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.


```r
imputedData <- mutate(imputedData, weekday=as.factor(ifelse(weekdays(date) %in% c('Saturday', 'Sunday'), 'weekend', 'weekday')))
```

Now we can make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days.


```r
weekdayData <- filter(imputedData, weekday=='weekday')
weekendData <- filter(imputedData, weekday=='weekend')

groupedWeekdayByInterval <- group_by(weekdayData, interval)
groupedWeekendByInterval <- group_by(weekendData, interval)

meanWeekdayStepsPerInterval <- summarise(groupedWeekdayByInterval, steps=mean(steps, na.rm=TRUE))
meanWeekendStepsPerInterval <- summarise(groupedWeekendByInterval, steps=mean(steps, na.rm=TRUE))

par(mfrow=c(2,1))
plot(meanWeekdayStepsPerInterval, type="l", main="weekday")
plot(meanWeekendStepsPerInterval, type="l", main="weekend")
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png) 

That concludes our analysis.
