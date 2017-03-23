# Reproducible Research: Peer Assessment 1

# Loading packages

```r
library(ggplot2)
library(lubridate)
```

```
## 
## Attaching package: 'lubridate'
```

```
## The following object is masked from 'package:base':
## 
##     date
```

# Loading and preprocessing the data

### Reading File

```r
unzip("activity.zip")
activityData <- read.csv("activity.csv")
```

### Change date into correct format

```r
activityData$date <- ymd(activityData$date)
head(activityData)
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

# Part 1: What is the mean total number of steps taken per day?

## A: Calculate the total number of steps taken per day and create a histogram of the results.

```r
StepsPerDay <-tapply(activityData$steps, activityData$date, FUN = sum, na.rm=TRUE) 
hist(StepsPerDay, 
     xlab = "Total Number of Steps",
     ylab = "Frequency",
     main = "The Total Number of Steps Taken Per Day",
     col = "green")
```

![](Coursera_Reproducible_Research_Project_1_files/figure-html/histogram1-1.png)<!-- -->

## B: Calculate and report the mean and median of the total number of steps taken per day.
### Mean

```r
mean(StepsPerDay, na.rm=TRUE)
```

```
## [1] 9354.23
```

### Median

```r
median(StepsPerDay, na.rm=TRUE)
```

```
## [1] 10395
```

# Part 2: What is the average daily activity pattern?

## A: Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
averageSteps <- aggregate(x=list(steps=activityData$steps), by=list(interval=activityData$interval),
    FUN=mean, na.rm=TRUE)
```


```r
ggplot(data=averageSteps, aes(x=interval, y=steps)) +
    geom_line(color="blue") +
    xlab("5-Minute Interval") +
    ylab("Average Number of Steps Taken") +
    ggtitle("Average Daily Activity Pattern") 
```

![](Coursera_Reproducible_Research_Project_1_files/figure-html/Plot-1.png)<!-- -->

## B: Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
averageSteps[which.max(averageSteps$steps),]
```

```
##     interval    steps
## 104      835 206.1698
```

# Part 3: Inputing missing values.

## A: Calculate and report the total number of missing values in the dataset.

```r
missingValues <- is.na(activityData$steps)
table(missingValues)
```

```
## missingValues
## FALSE  TRUE 
## 15264  2304
```

## B: Fill the missing values in the dataset.

```r
ReplacemissingValues <- function(steps, interval) {
    filled <- NA
    if (!is.na(steps)) 
        filled <- c(steps) else filled <- (averageSteps[averageSteps$interval == interval, "steps"])
    return(filled)
}
fillData <- activityData
fillData$steps <- mapply(ReplacemissingValues, fillData$steps, fillData$interval)
```

## C: Create a new dataset that is equal to the original dataset but with the missing data filled in.


## D: Make a histogram of the total number of steps taken each day and calcualte the mean and median total number of steps taken per day.

```r
totalSteps <-tapply(fillData$steps, fillData$date, FUN = sum, na.rm=TRUE) 
hist(totalSteps, 
     xlab = "Total Number of Steps",
     ylab = "Frequency",
     main = "The Total Number of Steps Taken Per Day (Missing Data Filled In)",
     col = "orange")
```

![](Coursera_Reproducible_Research_Project_1_files/figure-html/histogram2-1.png)<!-- -->

### Mean

```r
mean(totalSteps)
```

```
## [1] 10766.19
```

### Median

```r
median(totalSteps)
```

```
## [1] 10766.19
```

# Part 4: Are there differences in activity patterns between weekdays and weekends?

## A: Create a new factor variable in the dataset with two levels - "weekday" and "weekend".

```r
weekday.or.weekend <- function(date) {
    day <- weekdays(date)
    if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")) 
        return("weekday") else if (day %in% c("Saturday", "Sunday")) 
        return("weekend") else stop("invalid date")
}
fillData$date <- as.Date(fillData$date)
fillData$day <- sapply(fillData$date, FUN = weekday.or.weekend)
```

## B: Make a panel plot containing the time series plit of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
averageNumbersteps <- aggregate(steps ~ interval + day, data = fillData, mean)
ggplot(averageNumbersteps, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) + 
    xlab("5-Minute Interval") + ylab("Number of Steps")
```

![](Coursera_Reproducible_Research_Project_1_files/figure-html/unnamed-chunk-13-1.png)<!-- -->
