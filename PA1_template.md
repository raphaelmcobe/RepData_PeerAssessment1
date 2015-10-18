# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the datatappl


```r
library (lattice)
options(scipen = 1, digits = 2)

unzip("activity.zip")
dataset = read.csv("activity.csv")
```

## What is mean total number of steps taken per day?

For this part of the assignment, you can ignore the missing values in the dataset.

1. Calculate the total number of steps taken per day

2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day

3. Calculate and report the mean and median of the total number of steps taken per day


```r
steps.total = aggregate(dataset$steps, list(dataset$date), sum, na.rm = T)

hist(steps.total$x, main = "Frequency of number of steps taken each day", 
     xlab = "Total number of steps", breaks = 20)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
steps.total.mean = mean(steps.total$x, na.rm = T)
steps.total.median = median(steps.total$x, na.rm = T)
```
The mean of the total number of steps taken per day is 9354.23.  
The median of the total number of steps taken per day is 10395.

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?


```r
steps.average = aggregate(dataset$steps, list(dataset$interval), mean, na.rm=TRUE)
plot(steps.average$Group.1, steps.average$x, type="l", 
     main = "Interval X Average number of steps", 
     xlab = "Interval", ylab = "Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
steps.max = max(steps.average$x)
interval.max = subset(steps.average,steps.average$x == steps.max)$Group.1
```

The 5-minute interval that contains the maximum number of steps (on average) is 835 
(with average of 206.17 steps).


## Imputing missing values
Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

3. Create a new dataset that is equal to the original dataset but with the missing data filled in.

4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?  


```r
na.total = nrow(dataset[is.na(dataset$steps),])
```
  
The number of missing values in the dataset is 2304.  
  

```r
dataset2 = data.frame(steps = dataset$steps, date = dataset$date, interval = dataset$interval)
steps.mean = tapply(dataset2$steps,dataset2$interval, mean, na.rm = TRUE)
for(i in 1:nrow(dataset)){
    if(is.na(dataset2$steps[i])){
        dataset2$steps[i] = steps.mean[as.character(dataset2$interval[i])]
    }
}


steps.total2 = aggregate(dataset2$steps, list(dataset2$date), sum)
hist(steps.total2$x, breaks = 20,
     main = "Frequency of number of steps taken each day",
     xlab = "Total number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

Comparison between histograms:


```r
hist(steps.total2$x, col=rgb(1,0,0,1/2), breaks = 20,
     main = "Frequency of number of steps taken each day",
     xlab = "Total number of steps")
hist(steps.total$x, col=rgb(0,0,1,1/2), breaks = 20, 
     main = "Frequency of  number of steps taken each day",
     xlab = "Total number of steps",
     add = T)
legend("topright", c("Data as is", "Data with imputed NA"), col=c(rgb(1,0,0,1/2),rgb(0,0,1,1/2)), lwd=10)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

```r
steps.total2.mean = mean(steps.total2$x)
steps.total2.median = median(steps.total2$x)
```

The new mean of the total number of steps taken per day is 10766.19.  
The new median of the total number of steps taken per day is 10766.19.


## Are there differences in activity patterns between weekdays and weekends?
For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.


```r
dataset$day.type = "weekday"
dataset$date <- as.Date(dataset$date, "%Y-%m-%d")

for(i in 1:nrow(dataset)){
    if(weekdays(as.Date(dataset[i,]$date)) == "sábado" || 
        weekdays(as.Date(dataset[i,]$date)) == "domingo"){
        dataset[i,]$day.type = "weekend"
    }
}
dataset$day.type = as.factor(dataset$day.type)

dataset.filtered <- aggregate(dataset$steps, list(dataset$interval, dataset$day.type),mean, na.rm = T)


xyplot(x ~ Group.1 | Group.2, data=dataset.filtered, layout = c(1,2), type = c("l"), 
       xlab = "Interval", ylab = "Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 
