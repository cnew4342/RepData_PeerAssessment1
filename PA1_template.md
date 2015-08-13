Peer Assessment Project #1 for Reproducible Research
==============================================================================

# Activity Data on Steps Per Day within 5 Minute Intervals
---
title: "PA1_template.Rmd"
author: "Carl Newman"
date: "August 8, 2015"
output: 
  html_document: 
    keep_md: yes
---

## Loading and preprocessing the data
* Download data package.  
* Unzip package.  
* Read activity.csv File & display file structure.  

```r
url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
download(url, dest="dataset.zip", mode="wb") 
unzip ("dataset.zip", exdir = "./")
actdata <- read.csv("activity.csv")
str(actdata)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
summary(actdata)
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
## What is mean total number of steps taken per day?

```r
sumdate <- group_by(actdata, date) %>%
        summarize(steps=sum(steps, na.rm = TRUE))
s <- summary(sumdate,na.rm = TRUE)
ggplot(sumdate, aes(x=steps)) +
        geom_histogram(binwidth=1000, colour="black", fill="#66cccc") +
        geom_vline(aes(xintercept=mean(steps, na.rm=TRUE)),   # Ignore NA values for mean
                   color="red", linetype="dashed", size=1) +
        ggtitle("Histogram of Daily Steps Taken\nwith Mean (red dashed line)")
```

![plot of chunk chunk2](figure/chunk2-1.png) 

```r
cat("Number of Steps per Day =", s[3:4,2])
```

```
## Number of Steps per Day = Median :10395   Mean   : 9354
```
## What is the average daily activity pattern?
As demonstrated by plotting the 5-minute interval and the average number of steps taken.  

```r
actintv <- group_by(actdata, interval)
sai <- summarize(actintv, steps = mean(steps, na.rm=TRUE))
max <- sai$steps == max(sai$steps)
ggplot(sai, aes(x = interval, y = steps)) + geom_line(color = "red") +
        geom_vline(aes(xintercept = sai$interval[max == TRUE]),   # Ignore NA values for mean
                   color="blue", linetype="dashed", size=1) +
        ggtitle("Mean Daily Steps Taken by Interval\nwith Most Frequent Interval (blue dashed line)")
```

![plot of chunk chunk3](figure/chunk3-1.png) 

```r
cat("Interval with Most Steps per Day =", sai$interval[max == TRUE], 
    "Mean Number of Steps =", max(sai$steps))
```

```
## Interval with Most Steps per Day = 835 Mean Number of Steps = 206.1698
```
## Analysing missing values
* Missing values in steps do not appear to be concentrated in certain intervals.  
* Missing values in steps are not related to day of week.  
* Missing values in steps are not related to month. 


```r
actdata2 <- mutate(actdata, dt = ymd(as.character(actdata$date)),
                   missing = is.na(actdata$steps), mdt = month(dt, label = TRUE),
                   ddt = wday(dt, label = TRUE))
```


```r
boxplot(interval ~ missing, data = actdata2, col = "lightgray", 
        main = "Intervals for Steps missing & non-missing", 
        xlab = "Non-missing v Missing")
```

![plot of chunk chunk5](figure/chunk5-1.png) 


```r
ggplot(actdata2, aes(x=ddt, fill = missing)) +
        geom_histogram(binwidth=.5, position="dodge") +
        ggtitle("Missing v. Non-missing Intervals by Day of Week")
```

![plot of chunk chunk6](figure/chunk6-1.png) 


```r
ggplot(actdata2, aes(x=mdt, fill = missing)) +
        geom_histogram(binwidth=.5, position="dodge") +
        ggtitle("Missing v. Non-missing Intervals by Month")
```

![plot of chunk chunk7](figure/chunk7-1.png) 

##  Impute missing values for steps as mean of steps across same day of week plus mean of steps across same interval divided by 2.  

```r
actint <- group_by(actdata2, interval)
actddt <- group_by(actdata2, ddt)
sad <- summarize(actddt, mdsteps = mean(steps, na.rm=TRUE))
sai <- summarize(actint, misteps = mean(steps, na.rm=TRUE))
actdatai <- merge(x = actdata2, y = sad, by = "ddt", all.x = TRUE) %>%
        merge(y = sai, by = "interval", all.x = TRUE) %>%
        mutate(steps = ifelse(is.na(steps), (mdsteps + misteps) / 2, steps)) %>%
        select(steps, date, interval)
```

## What is mean total number of steps taken per day after imputation of missing values?
* Distribution is not noticeably changed.
* Median unchanged, mean very close to unimputed value


```r
sumdate2 <- group_by(actdatai, date) %>%
        summarize(steps=sum(steps, na.rm = TRUE))
si <- summary(sumdate2, na.rm = TRUE)
ggplot(sumdate2, aes(x=steps)) +
        geom_histogram(binwidth=500, colour="black", fill="cyan") +
        geom_vline(aes(xintercept=mean(steps, na.rm=TRUE)),   # Ignore NA values for mean
                   color="red", linetype="dashed", size=1) +
        ggtitle("Histogram of Daily Steps Taken (missing values imputed)\nwith Mean (red dashed line)")
```

![plot of chunk chunk9](figure/chunk9-1.png) 

```r
cat("Number of Steps per Day =", si[3:4,2])
```

```
## Number of Steps per Day = Median :11015   Mean   :10794
```

## Are there differences in activity patterns between weekdays and weekends?

```r
actdataf <- mutate(actdata2, 
                   ddttype = ifelse (ddt == "Sun" | ddt == "Sat", "weekend", "weekday")) 
ggplot(actdataf, aes(x = interval, y = steps)) + geom_line(color = "green") +
        ggtitle("Mean Daily Steps Taken by Interval\nfor Weekdays & Weekends") +
        facet_grid(ddttype ~ .)
```

```
## Warning: Removed 2 rows containing missing values (geom_path).
```

![plot of chunk chunk10](figure/chunk10-1.png) 

```r
last <- group_by(actdataf, ddttype)
summarize(last, mean=mean(steps, na.rm = TRUE))
```

```
## Source: local data frame [2 x 2]
## 
##   ddttype     mean
## 1 weekday 35.33796
## 2 weekend 43.07837
```
