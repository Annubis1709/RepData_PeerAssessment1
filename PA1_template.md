---
title: "Reproducible Research: Peer Assessment 1"
author: "Edier Sanchez Sanchez"
date: "09/13/2020"
output: 
  html_document:
    keep_md: true
---

# Settings


```r
library(lattice)
```




## Loading and preprocessing the data



```r
if (!file.exists("activity.csv") )
    {
     file_Url <- 'https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip'  
     download.file(file_Url, destfile = 'repdata%2Fdata%2Factivity.zip', mode = 'wb')  
     unzip('repdata%2Fdata%2Factivity.zip')
    }

data <- read.csv("activity.csv")  
```



## What is mean total number of steps taken per day?


1. Calculate the total number of steps taken per day:


```r
total_steps <- aggregate(steps ~ date, data, sum)
total_steps
```

```
##          date steps
## 1  2012-10-02   126
## 2  2012-10-03 11352
## 3  2012-10-04 12116
## 4  2012-10-05 13294
## 5  2012-10-06 15420
## 6  2012-10-07 11015
## 7  2012-10-09 12811
## 8  2012-10-10  9900
## 9  2012-10-11 10304
## 10 2012-10-12 17382
## 11 2012-10-13 12426
## 12 2012-10-14 15098
## 13 2012-10-15 10139
## 14 2012-10-16 15084
## 15 2012-10-17 13452
## 16 2012-10-18 10056
## 17 2012-10-19 11829
## 18 2012-10-20 10395
## 19 2012-10-21  8821
## 20 2012-10-22 13460
## 21 2012-10-23  8918
## 22 2012-10-24  8355
## 23 2012-10-25  2492
## 24 2012-10-26  6778
## 25 2012-10-27 10119
## 26 2012-10-28 11458
## 27 2012-10-29  5018
## 28 2012-10-30  9819
## 29 2012-10-31 15414
## 30 2012-11-02 10600
## 31 2012-11-03 10571
## 32 2012-11-05 10439
## 33 2012-11-06  8334
## 34 2012-11-07 12883
## 35 2012-11-08  3219
## 36 2012-11-11 12608
## 37 2012-11-12 10765
## 38 2012-11-13  7336
## 39 2012-11-15    41
## 40 2012-11-16  5441
## 41 2012-11-17 14339
## 42 2012-11-18 15110
## 43 2012-11-19  8841
## 44 2012-11-20  4472
## 45 2012-11-21 12787
## 46 2012-11-22 20427
## 47 2012-11-23 21194
## 48 2012-11-24 14478
## 49 2012-11-25 11834
## 50 2012-11-26 11162
## 51 2012-11-27 13646
## 52 2012-11-28 10183
## 53 2012-11-29  7047
```

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day:



```r
hist(total_steps$steps, main = paste("Total Steps Each Day"),  col="yellow", xlab = "Number of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

3. Calculate and report the mean and median of the total number of steps taken per day:



```r
mean_steps <- mean(total_steps$steps)
mean_steps
```

```
## [1] 10766.19
```


```r
median_steps <- median(total_steps$steps)
median_steps
```

```
## [1] 10765
```


## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "1") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis):


```r
steps_by_interval <- aggregate(steps ~ interval, data, mean)
plot(steps_by_interval$interval, steps_by_interval$steps, type = "l", xlab = "Interval", ylab = "Number of Steps", main = "Average Number of Steps per Day by Interval")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?:


```r
max_number_steps <- steps_by_interval[which.max(steps_by_interval$steps),1]
max_number_steps
```

```
## [1] 835
```



## Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)


```r
total_NAs <- sum(!complete.cases(data))
total_NAs
```

```
## [1] 2304
```

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.


```r
average_steps <- aggregate(steps ~ interval, data = data, FUN = mean)
fill_NAs <- numeric()

for (i in 1:nrow(data)) {
    data_obs <- data[i, ]
    
    if (is.na(data_obs$steps)) {
        steps <- subset(average_steps, interval == data_obs$interval)$steps
    } else {
      
        steps <- data_obs$steps
    }
    
    fill_NAs <- c(fill_NAs, steps)
}
```

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.


```r
new_dataset <- data
new_dataset$steps <- fill_NAs
```

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
totalSteps_by_day <- aggregate(steps ~ date, data = new_dataset, 
                               sum, na.rm = TRUE)

hist(totalSteps_by_day$steps, main = paste("Total Steps Each Day"), 
     col = "red", xlab = "Number of Steps")

hist(total_steps$steps, main = paste("Total Steps Each Day"), 
     col = "yellow", xlab = "Number of Steps", add = T)
legend("topright", c("Imputed", "Non-imputed"), col = c("red", "yellow"), 
       lwd = 10)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->


```r
total_mean <- mean(totalSteps_by_day$steps)
total_mean
```

```
## [1] 10766.19
```


```r
total_median <- median(totalSteps_by_day$steps)
total_median
```

```
## [1] 10766.19
```


```r
desviation_meand <- total_mean - mean_steps
desviation_meand
```

```
## [1] 0
```


```r
desviation_median <- total_median - median_steps
desviation_median
```

```
## [1] 1.188679
```




## Are there differences in activity patterns between weekdays and weekends?

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.



```r
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
new_dataset$dow = as.factor(ifelse(is.element(weekdays(as.Date(new_dataset$date)), weekdays), "Weekday", "Weekend"))
```


2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.



```r
totalSteps_by_day <- aggregate(steps ~ interval + dow, new_dataset, mean)
xyplot(totalSteps_by_day$steps ~ totalSteps_by_day$interval|totalSteps_by_day$dow, main = "Average Steps by Day by Interval", xlab = "Interval", ylab = "Steps", layout = c(1,2), type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-18-1.png)<!-- -->





