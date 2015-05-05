# Reproducible Research: Peer Assessment 1

The following code are used to perform data manipulations and ensuing analysis in order to answer the following questions.  

1. What is the mean total number of steps taken per day?
2. What is the average daily activity pattern?
3. Inputing the missing value  
        a. Report total number of NAs  
        b. Devise a strategy for filling in all of the missing values in the    dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
4. Are there differences in activity patterns between weekdays and weekends?

## Loading and preprocessing the data


```r
setwd("~/GitHub/RepData_PeerAssessment1")
unzip("activity.zip")

data <- read.csv("activity.csv")

data$date <- as.Date(data$date)
```

## Loading necessary packages


```r
library(plyr)
library(ggplot2)
```


## What is mean total number of steps taken per day?


```r
library(plyr)
total <- ddply(data,"date",summarize, sum = sum(steps))


plot(total$date,total$sum, type="h")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
#average
mean(total$sum,na.rm=TRUE)
```

```
## [1] 10766.19
```

```r
#median
median(total$sum,na.rm=TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?


```r
inter <- ddply(data,"interval",summarize, TotalSteps = sum(steps,na.rm=TRUE))

plot(inter$interval,inter$TotalSteps, type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png) 

## Imputing missing values


```r
# 1. Report total number of NAs 
sum(is.na(data))
```

```
## [1] 2304
```

```r
# 2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

boom <- ddply(data, .(interval), transform, steps=ifelse(is.na(steps), median(steps, na.rm=TRUE), steps))

blah <- ddply(boom,"date",summarize, sum = sum(steps))
plot(blah$date,blah$sum, type="h")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png) 

```r
#average
mean(blah$sum,na.rm=TRUE)
```

```
## [1] 9503.869
```

```r
#median
median(blah$sum,na.rm=TRUE)
```

```
## [1] 10395
```

## Are there differences in activity patterns between weekdays and weekends?


```r
DayType <- data
DayType$days <- weekdays(as.Date(DayType$date))
DayType$daytype[DayType$days == "Monday" | DayType$days == "Tuesday" | DayType$days == "Wednesday" | DayType$days == "Thursday" | DayType$days == "Friday"] <- "Weekday"
DayType$daytype[DayType$days == "Saturday" | DayType$days == "Sunday"] <- "Weekend"


IntervalDay <- ddply(DayType,.(daytype,interval),summarize, MeanSteps = mean(steps,na.rm=TRUE))

qplot(interval,MeanSteps, data = IntervalDay, colour = daytype)
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

```r
qplot(interval,MeanSteps, data = IntervalDay, colour = daytype, geom = "smooth", method="auto")
```

```
## geom_smooth: method="auto" and size of largest group is <1000, so using loess. Use 'method = x' to change the smoothing method.
```

![](PA1_template_files/figure-html/unnamed-chunk-6-2.png) 
