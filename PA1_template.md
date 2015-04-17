# Reproducible Research - Project 1
Alex Alves  
April 13, 2015  

The activity dataset consists of:

* steps: Number of steps taking in a 5-minute interval (missing values are coded as NA)

* date: The date on which the measurement was taken in YYYY-MM-DD format

* interval: Identifier for the 5-minute interval in which measurement was taken

There are a total of 17,568 observations in this dataset, stored as CSV file.

# Loading and preprocessing the data

Firstly, we read the file, and take a quick look at it:




```r
activity <- read.csv("activity.csv")
summary(activity)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```

# What is mean total number of steps taken per day?

Next, let's calculate the daily mean and median number of steps.


```r
activity_per_day <- group_by(activity, date)
summarized_date <- summarize(activity_per_day, total_steps = sum(steps, na.rm = TRUE))
hist(summarized_date$total_steps, xlab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
mean(summarized_date$total_steps)
```

```
## [1] 9354.23
```

```r
median(summarized_date$total_steps)
```

```
## [1] 10395
```

# What is the average daily activity pattern?

Next, let's calculate a daily pattern of activity per interval of day:


```r
activity_per_interval <- group_by(activity, interval)
summarized_interval <- summarize(activity_per_interval, mean_steps = mean(steps, na.rm = TRUE))
plot(summarized_interval$interval, summarized_interval$mean_steps, type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

```r
max_interval <- which(summarized_interval$mean_steps == max(summarized_interval$mean_steps))
```

The interval with the most activity is 835.

# Imputing missing values


```r
num_nas <- sum(!complete.cases(activity))
```

The number of rows with missing values is 2304.


```r
imputated_activity <- mutate(activity, steps = ifelse(is.na(steps), round(summarized_interval$mean_steps), steps))
imputated_activity_per_day <- group_by(imputated_activity, date)
imputated_summarized_date <- summarize(imputated_activity_per_day, total_steps = sum(steps))
hist(imputated_summarized_date$total_steps, xlab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

```r
mean(imputated_summarized_date$total_steps)
```

```
## [1] 10765.64
```

```r
median(imputated_summarized_date$total_steps)
```

```
## [1] 10762
```

```r
par(mfrow = c(1, 2))
boxplot(summarized_date$total_steps)
boxplot(imputated_summarized_date$total_steps) 
```

![](PA1_template_files/figure-html/unnamed-chunk-6-2.png) 

The imputation of the data increases mean and median, and narrows and normalizes the distribution.

# Are there differences in activity patterns between weekdays and weekends?


```r
imputated_activity_weekday <- mutate(imputated_activity, day_of_week = factor(!weekdays(as.Date(activity$date)) == c("Saturday", "Sunday"), labels = c("weekday", "weekend")))
imputated_activity_weekday_interval <- group_by(imputated_activity_weekday, interval, day_of_week)
summarized_imputated_activity_weekday_interval <- summarize(imputated_activity_weekday_interval, mean_steps = mean(steps))
xyplot(mean_steps ~ interval | day_of_week, summarized_imputated_activity_weekday_interval, type = "l")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

As shown by the time-series plots, one does less activities during the weekend, particularly during the middle of the day.
