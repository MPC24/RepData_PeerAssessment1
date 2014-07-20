---
title: "Activity Monitoring - Steps"
author: "Michael Conway"
date: "Friday, July 18, 2014"
output: html_document
---
###Loading and preprocessing the data

Imported data via 'read.csv' while formatting the variables: 'steps' = numeric, date = as.Date, and 'interval' = integer. Second step was to create an object omitting NAs. Further formatting was performed at different stages of assignment based on the requirements of the task.


```r
amd <- read.csv(file = "amd.csv", header = TRUE, colClasses = c("numeric", as.Date("date",format = "%m/%d/%Y"), "integer" ))
amdn <- na.omit(amd)
```

###Mean steps per day

* Histogram
* Mean and Median
    + Summarize
    + Calculate


```r
library(ggplot2)
library(plyr)

ggplot(data = amdn, aes(x = date, y = steps)) + geom_histogram(fill = "red", stat = "identity") + theme(axis.text.x = element_text(angle = 90)) + xlab("Date") + ylab("Steps") + labs(title = "Steps per Day") + theme(plot.title = element_text(size = rel(2), vjust = 2)) + theme(axis.text = element_text(size = rel(.75))) + theme(axis.title = element_text(size = rel(1.5)))
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

```r
amdnm <- ddply(amdn,~date, summarize, mean=mean(steps),median=median(steps))
amdnm
```

```
##          date    mean median
## 1  10/10/2012 34.3750      0
## 2  10/11/2012 35.7778      0
## 3  10/12/2012 60.3542      0
## 4  10/13/2012 43.1458      0
## 5  10/14/2012 52.4236      0
## 6  10/15/2012 35.2049      0
## 7  10/16/2012 52.3750      0
## 8  10/17/2012 46.7083      0
## 9  10/18/2012 34.9167      0
## 10 10/19/2012 41.0729      0
## 11  10/2/2012  0.4375      0
## 12 10/20/2012 36.0938      0
## 13 10/21/2012 30.6285      0
## 14 10/22/2012 46.7361      0
## 15 10/23/2012 30.9653      0
## 16 10/24/2012 29.0104      0
## 17 10/25/2012  8.6528      0
## 18 10/26/2012 23.5347      0
## 19 10/27/2012 35.1354      0
## 20 10/28/2012 39.7847      0
## 21 10/29/2012 17.4236      0
## 22  10/3/2012 39.4167      0
## 23 10/30/2012 34.0938      0
## 24 10/31/2012 53.5208      0
## 25  10/4/2012 42.0694      0
## 26  10/5/2012 46.1597      0
## 27  10/6/2012 53.5417      0
## 28  10/7/2012 38.2465      0
## 29  10/9/2012 44.4826      0
## 30 11/11/2012 43.7778      0
## 31 11/12/2012 37.3785      0
## 32 11/13/2012 25.4722      0
## 33 11/15/2012  0.1424      0
## 34 11/16/2012 18.8924      0
## 35 11/17/2012 49.7882      0
## 36 11/18/2012 52.4653      0
## 37 11/19/2012 30.6979      0
## 38  11/2/2012 36.8056      0
## 39 11/20/2012 15.5278      0
## 40 11/21/2012 44.3993      0
## 41 11/22/2012 70.9271      0
## 42 11/23/2012 73.5903      0
## 43 11/24/2012 50.2708      0
## 44 11/25/2012 41.0903      0
## 45 11/26/2012 38.7569      0
## 46 11/27/2012 47.3819      0
## 47 11/28/2012 35.3576      0
## 48 11/29/2012 24.4688      0
## 49  11/3/2012 36.7049      0
## 50  11/5/2012 36.2465      0
## 51  11/6/2012 28.9375      0
## 52  11/7/2012 44.7326      0
## 53  11/8/2012 11.1771      0
```

###Average Daily Activity

* Subset by interval and caculate mean
    + Plot mean activity by interval
    + Identify max mean interval 


```r
ame <- ddply(amdn,~interval, summarize, mean=mean(steps))

ggplot(data = ame, aes(x = interval, y = mean)) + geom_line(size = 1, colour = "blue") + xlab("Interval(24hrs/5minutes)") + ylab("Average Steps") + labs(title = "Average Steps per Day for October and November") + theme(plot.title = element_text(size = rel(1.5), vjust = 2))+ theme(panel.background = element_rect(fill = "white", colour = "black"))
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 

```r
subset(ame, mean == max(ame$mean))
```

```
##     interval  mean
## 104      835 206.2
```

###Imputing Missing Values

* Sum NA's
* NA replacement strategy: use inerval means from 'ame' object to replace NA's in corresponding interval with mean
    + Create column equalling 'steps' then input inverval means into NA's with for loop
  
* Plot histogram
* Calculate mean and median of new data set with replaced NA's


```r
sum(is.na(amd$steps))
```

```
## [1] 2304
```

```r
amd$steps_imp <- amd$steps
for(i in 1:nrow(amd)){
    if(is.na(amd$steps[i])){
        amd$steps_imp[i] <- ame[ame$interval==amd$interval[i], 'mean']
    }
}

ggplot(data = amd, aes(x = date, y = steps_imp)) + geom_histogram(fill = "red", stat = "identity") + theme(axis.text.x = element_text(angle = 90)) + xlab("Date") + ylab("Steps") + labs(title = "Steps per Day") + theme(plot.title = element_text(size = rel(2), vjust = 2)) + theme(panel.background = element_rect(fill = "white", colour = "black")) + theme(axis.title=element_text(size=rel(1.5))) + theme(axis.text = element_text(size = rel(.75)))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 

```r
amedays <- ddply(amd,~date, summarize, mean=mean(steps_imp),median=median(steps_imp))
amedays
```

```
##          date    mean median
## 1   10/1/2012 37.3826  34.11
## 2  10/10/2012 34.3750   0.00
## 3  10/11/2012 35.7778   0.00
## 4  10/12/2012 60.3542   0.00
## 5  10/13/2012 43.1458   0.00
## 6  10/14/2012 52.4236   0.00
## 7  10/15/2012 35.2049   0.00
## 8  10/16/2012 52.3750   0.00
## 9  10/17/2012 46.7083   0.00
## 10 10/18/2012 34.9167   0.00
## 11 10/19/2012 41.0729   0.00
## 12  10/2/2012  0.4375   0.00
## 13 10/20/2012 36.0938   0.00
## 14 10/21/2012 30.6285   0.00
## 15 10/22/2012 46.7361   0.00
## 16 10/23/2012 30.9653   0.00
## 17 10/24/2012 29.0104   0.00
## 18 10/25/2012  8.6528   0.00
## 19 10/26/2012 23.5347   0.00
## 20 10/27/2012 35.1354   0.00
## 21 10/28/2012 39.7847   0.00
## 22 10/29/2012 17.4236   0.00
## 23  10/3/2012 39.4167   0.00
## 24 10/30/2012 34.0938   0.00
## 25 10/31/2012 53.5208   0.00
## 26  10/4/2012 42.0694   0.00
## 27  10/5/2012 46.1597   0.00
## 28  10/6/2012 53.5417   0.00
## 29  10/7/2012 38.2465   0.00
## 30  10/8/2012 37.3826  34.11
## 31  10/9/2012 44.4826   0.00
## 32  11/1/2012 37.3826  34.11
## 33 11/10/2012 37.3826  34.11
## 34 11/11/2012 43.7778   0.00
## 35 11/12/2012 37.3785   0.00
## 36 11/13/2012 25.4722   0.00
## 37 11/14/2012 37.3826  34.11
## 38 11/15/2012  0.1424   0.00
## 39 11/16/2012 18.8924   0.00
## 40 11/17/2012 49.7882   0.00
## 41 11/18/2012 52.4653   0.00
## 42 11/19/2012 30.6979   0.00
## 43  11/2/2012 36.8056   0.00
## 44 11/20/2012 15.5278   0.00
## 45 11/21/2012 44.3993   0.00
## 46 11/22/2012 70.9271   0.00
## 47 11/23/2012 73.5903   0.00
## 48 11/24/2012 50.2708   0.00
## 49 11/25/2012 41.0903   0.00
## 50 11/26/2012 38.7569   0.00
## 51 11/27/2012 47.3819   0.00
## 52 11/28/2012 35.3576   0.00
## 53 11/29/2012 24.4688   0.00
## 54  11/3/2012 36.7049   0.00
## 55 11/30/2012 37.3826  34.11
## 56  11/4/2012 37.3826  34.11
## 57  11/5/2012 36.2465   0.00
## 58  11/6/2012 28.9375   0.00
## 59  11/7/2012 44.7326   0.00
## 60  11/8/2012 11.1771   0.00
## 61  11/9/2012 37.3826  34.11
```

*Summary Analysis of impact of imputing inverval means for NA's
    + Means did not change
    + Where days with all NA's replaced with mean intervals, median was 34.11; all other medians per day in both instances of monitoring where zero
    + NA replacement improves graphic appearance without skewing data summary
    
###Weekdays and Weekends

* Create factor with for loop
* Subset by factor and calculate mean
* Create panel plot seperated by factors weekday and weekend
    

```r
amd$dates <- as.Date(amd$date)
for(i in 1:nrow(amd)){
    if(weekdays(amd$dates[i]) %in% c("Sunday", "Saturday")) {
        amd$day[i] <- "weekend"
    } else amd$day[i] <- "weekday"
}

amem <- ddply(amd, .(interval, day), summarize, mean=mean(steps_imp))

ggplot(data = amem, aes(x = interval, y = mean)) + geom_line(size = 1, colour = "blue") + xlab("Interval(24hrs/5minutes)") + ylab("Average Steps") + labs(title = "Average Steps per Day for October and November") + theme(plot.title = element_text(size = rel(1.5), vjust = 2))+ theme(panel.background = element_rect(fill = "white", colour = "black")) + facet_grid(day ~ .)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

