library(dplyr)
# Loading and preprocessing the data

unzip('activity.zip')
data <- read.csv('activity.csv', colClasses=c('numeric', 'Date', 'numeric'))

# What is mean total number of steps taken per day (ignore N/A)?

# 1. Make a histogram of the total number of steps taken each day 
groupedByDay <- group_by(data, date)
stepsPerDay <- summarise(groupedByDay, steps=sum(steps, na.rm=TRUE))
hist(stepsPerDay$steps)

# 2. Calculate and report the mean and median total number of steps taken per day
mean(stepsPerDay$steps)
median(stepsPerDay$steps)

# What is the average daily activity pattern?

# 1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
groupedByInterval <- group_by(data, interval)
meanStepsPerInterval <- summarise(groupedByInterval, steps=mean(steps, na.rm=TRUE))
plot(meanStepsPerInterval, type="l")

# 2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
meanStepsPerInterval[meanStepsPerInterval$steps == max(meanStepsPerInterval$steps),]

# Inputing missing values

# Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

# 1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
sum(is.na(data$steps))

# 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.? 

# Strategy is to use the mean over the entire dataset

# 3. Create a new dataset that is equal to the original dataset but with the missing data filled in.
impute.mean <- function(x) replace(x, is.na(x), mean(x, na.rm=TRUE))
imputedData <- data
imputedData$steps <- impute.mean(data$steps)

# 4(a). Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day.
groupedByDay <- group_by(imputedData, date)
stepsPerDay <- summarise(groupedByDay, steps=sum(steps, na.rm=TRUE))
hist(stepsPerDay$steps)
mean(stepsPerDay$steps)
median(stepsPerDay$steps)

# 4(b). Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

# Are there differences in activity patterns between weekdays and weekends?

# 1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
imputedData <- mutate(imputedData, weekday=as.factor(ifelse(weekdays(date) %in% c('Saturday', 'Sunday'), 'weekend', 'weekday')))

# 2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
weekdayData <- filter(imputedData, weekday=='weekday')
weekendData <- filter(imputedData, weekday=='weekend')

groupedWeekdayByInterval <- group_by(weekdayData, interval)
groupedWeekendByInterval <- group_by(weekendData, interval)

meanWeekdayStepsPerInterval <- summarise(groupedWeekdayByInterval, steps=mean(steps, na.rm=TRUE))
meanWeekendStepsPerInterval <- summarise(groupedWeekendByInterval, steps=mean(steps, na.rm=TRUE))

par(mfrow=c(2,1))
plot(meanWeekdayStepsPerInterval, type="l", main="weekday")
plot(meanWeekendStepsPerInterval, type="l", main="weekend")