# Reproducible Research: Peer Assessment 1

library(lubridate)
library(knitr)
library(markdown)

## Loading and preprocessing the data


```r
activity<- read.csv("C:/Users/jorr/Documents/R/activity.csv")
```


## What is mean total number of steps taken per day?

```r
total_of_steps_per_day <- sum(activity$steps, na.rm = TRUE)
total_of_steps_per_day 
```

```
## [1] 570608
```

```r
total_steps_each_day <- aggregate(steps~date, data=activity, FUN=sum, na.rm=TRUE)
```



```r
hist(total_steps_each_day$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
total_steps_each_day_mean <- mean(total_steps_each_day$steps)
total_steps_each_day_median <- median(total_steps_each_day$steps)
total_steps_each_day_mean
```

```
## [1] 10766.19
```

```r
total_steps_each_day_median 
```

```
## [1] 10765
```


## What is the average daily activity pattern?

```r
five_minutes_average <- aggregate(steps~interval, data=activity, FUN=mean, na.rm=TRUE)
plot(x = five_minutes_average$interval, y = five_minutes_average$steps, type = "l") 
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

```r
max_steps <- max(five_minutes_average$steps)
for (i in 1:288) 
{
    if (five_minutes_average$steps[i] == max_steps)
        five_minute_interval_at_max_steps <- five_minutes_average$interval[i]
}
five_minute_interval_at_max_steps
```

```
## [1] 835
```



## Imputing missing values

```r
total_na <- 0
for (i in 1:17568)
{
    if(is.na(activity$steps[i])) 
        total_na <- total_na+1 
}
total_na
```

```
## [1] 2304
```





```r
##Fill in with 5 min interval mean
activity_filled_in <- activity
for (i in 1:17568) ## loop to find the na

    if(is.na(activity_filled_in$steps[i])) ## if steps is na store the pointer 
     
        five_minute_pointer <- activity_filled_in$interval[i] ##store the value of pointer to find the mean on five minute interval
        for (j in 1:288)  # loop to find the value of pointer on the data frame of five minute interval
        
            if (five_minutes_average$interval[j] == five_minute_pointer) ## finding the value of mean of five minute interval data frame
                activity_filled_in$steps[i] <- five_minutes_average$steps[j] ## replacing the na by the mean in that fime minute interval 
```



```r
## Calculate the total number of steps taken each day
total_steps_each_day_filled_in <- aggregate(steps~date, data=activity_filled_in, FUN=sum, na.rm=TRUE)


## Generate the Histogram by each day
hist(total_steps_each_day_filled_in$steps)
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
total_steps_each_day_mean_filled_in <- mean(total_steps_each_day_filled_in$steps)
total_steps_each_day_median_filled_in <- median(total_steps_each_day_filled_in$steps)
total_steps_each_day_mean_filled_in
```

```
## [1] 10566.83
```

```r
total_steps_each_day_median_filled_in
```

```
## [1] 10682.5
```



## Are there differences in activity patterns between weekdays and weekends?

 

```r
## create vector (week_day) 
week_day <- function(date_val) {
    wd <- weekdays(as.Date(date_val, '%Y-%m-%d'))
    if  (!(wd == 'Saturday' || wd == 'Sunday')) {
        x <- 'Weekday'
    } else {
        x <- 'Weekend'
    }
    x
}

# Apply the week_day function and add a new column to activity dataset
activity$day_type <- as.factor(sapply(activity$date, week_day))

#load the ggplot library
library(ggplot2)
```

```
## Warning: package 'ggplot2' was built under R version 3.3.2
```

```r
# Create the aggregated data frame by intervals and day_type
steps_per_day_impute <- aggregate(steps ~ interval+day_type, activity, mean)

# Create the plot
plt <- ggplot(steps_per_day_impute, aes(interval, steps)) +
    geom_line(stat = "identity", aes(colour = day_type)) +
    theme_gray() +
    facet_grid(day_type ~ ., scales="fixed", space="fixed") +
    labs(x="Time", y=expression("Steps")) +
    ggtitle("Number of steps vs. Time interval") + theme(legend.position="none")
print(plt)
```

![](PA1_template_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

