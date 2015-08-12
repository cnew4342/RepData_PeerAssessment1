---
title: "PA1_template.Rmd"
author: "Carl Newman"
date: "August 8, 2015"
output: html_document
---

```
## Error in library(ggplot2): there is no package called 'ggplot2'
```

```
## Error in library(dplyr): there is no package called 'dplyr'
```

```
## Error in library(lubridate): there is no package called 'lubridate'
```
## Loading and preprocessing the data
Read activity.csv File & display file structure.

```r
actdata <- read.csv("activity.csv")
```

```
## Warning in file(file, "rt"): cannot open file 'activity.csv': No such file
## or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```

```r
str(actdata)
```

```
## Error in str(actdata): object 'actdata' not found
```

```r
summary(actdata)
```

```
## Error in summary(actdata): object 'actdata' not found
```
## What is mean total number of steps taken per day?

```r
sumdate <- group_by(actdata, date) %>%
        summarize(steps=sum(steps, na.rm = TRUE))
```

```
## Error in eval(expr, envir, enclos): could not find function "%>%"
```

```r
s <- summary(sumdate,na.rm = TRUE)
```

```
## Error in summary(sumdate, na.rm = TRUE): object 'sumdate' not found
```

```r
ggplot(sumdate, aes(x=steps)) +
        geom_histogram(binwidth=1000, colour="black", fill="#66cccc") +
        geom_vline(aes(xintercept=mean(steps, na.rm=TRUE)),   # Ignore NA values for mean
                   color="red", linetype="dashed", size=1) +
        ggtitle("Histogram of Daily Steps Taken\nwith Mean (red dashed line)")
```

```
## Error in eval(expr, envir, enclos): could not find function "ggplot"
```

```r
cat("Number of Steps per Day =", s[3:4,2])
```

```
## Error in cat("Number of Steps per Day =", s[3:4, 2]): object 's' not found
```
## What is the average daily activity pattern?
As demonstrated by plotting the 5-minute interval and the average number of steps taken.  

```r
actintv <- group_by(actdata, interval)
```

```
## Error in eval(expr, envir, enclos): could not find function "group_by"
```

```r
sai <- summarize(actintv, steps = mean(steps, na.rm=TRUE))
```

```
## Error in eval(expr, envir, enclos): could not find function "summarize"
```

```r
max <- sai$steps == max(sai$steps)
```

```
## Error in eval(expr, envir, enclos): object 'sai' not found
```

```r
ggplot(sai, aes(x = interval, y = steps)) + geom_line(color = "red") +
        geom_vline(aes(xintercept = sai$interval[max == TRUE]),   # Ignore NA values for mean
                   color="blue", linetype="dashed", size=1) +
        ggtitle("Mean Daily Steps Taken by Interval\nwith Most Frequent Interval (blue dashed line)")
```

```
## Error in eval(expr, envir, enclos): could not find function "ggplot"
```

```r
cat("Interval with Most Steps per Day =", sai$interval[max == TRUE], 
    "Mean Number of Steps =", max(sai$steps))
```

```
## Error in cat("Interval with Most Steps per Day =", sai$interval[max == : object 'sai' not found
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

```
## Error in eval(expr, envir, enclos): could not find function "mutate"
```


```r
boxplot(interval ~ missing, data = actdata2, col = "lightgray", 
        main = "Intervals for Steps missing & non-missing", 
        xlab = "Non-missing v Missing")
```

```
## Error in eval(expr, envir, enclos): object 'actdata2' not found
```


```r
ggplot(actdata2, aes(x=ddt, fill = missing)) +
        geom_histogram(binwidth=.5, position="dodge") +
        ggtitle("Missing v. Non-missing Intervals by Day of Week")
```

```
## Error in eval(expr, envir, enclos): could not find function "ggplot"
```


```r
ggplot(actdata2, aes(x=mdt, fill = missing)) +
        geom_histogram(binwidth=.5, position="dodge") +
        ggtitle("Missing v. Non-missing Intervals by Month")
```

```
## Error in eval(expr, envir, enclos): could not find function "ggplot"
```

##  Impute missing values for steps as mean of steps across same day of week plus mean of steps across same interval divided by 2.  

```r
actint <- group_by(actdata2, interval)
```

```
## Error in eval(expr, envir, enclos): could not find function "group_by"
```

```r
actddt <- group_by(actdata2, ddt)
```

```
## Error in eval(expr, envir, enclos): could not find function "group_by"
```

```r
sad <- summarize(actddt, mdsteps = mean(steps, na.rm=TRUE))
```

```
## Error in eval(expr, envir, enclos): could not find function "summarize"
```

```r
sai <- summarize(actint, misteps = mean(steps, na.rm=TRUE))
```

```
## Error in eval(expr, envir, enclos): could not find function "summarize"
```

```r
actdatai <- merge(x = actdata2, y = sad, by = "ddt", all.x = TRUE) %>%
        merge(y = sai, by = "interval", all.x = TRUE) %>%
        mutate(steps = ifelse(is.na(steps), (mdsteps + misteps) / 2, steps)) %>%
        select(steps, date, interval)
```

```
## Error in eval(expr, envir, enclos): could not find function "%>%"
```

## What is mean total number of steps taken per day after imputation of missing values?
* Distribution is not noticeably changed.
* Median unchanged, mean very close to unimputed value


```r
sumdate2 <- group_by(actdatai, date) %>%
        summarize(steps=sum(steps, na.rm = TRUE))
```

```
## Error in eval(expr, envir, enclos): could not find function "%>%"
```

```r
si <- summary(sumdate2, na.rm = TRUE)
```

```
## Error in summary(sumdate2, na.rm = TRUE): object 'sumdate2' not found
```

```r
ggplot(sumdate2, aes(x=steps)) +
        geom_histogram(binwidth=500, colour="black", fill="cyan") +
        geom_vline(aes(xintercept=mean(steps, na.rm=TRUE)),   # Ignore NA values for mean
                   color="red", linetype="dashed", size=1) +
        ggtitle("Histogram of Daily Steps Taken (missing values imputed)\nwith Mean (red dashed line)")
```

```
## Error in eval(expr, envir, enclos): could not find function "ggplot"
```

```r
cat("Number of Steps per Day =", si[3:4,2])
```

```
## Error in cat("Number of Steps per Day =", si[3:4, 2]): object 'si' not found
```

## Are there differences in activity patterns between weekdays and weekends?

```r
actdataf <- mutate(actdata2, 
                   ddttype = ifelse (ddt == "Sun" | ddt == "Sat", "weekend", "weekday")) 
```

```
## Error in eval(expr, envir, enclos): could not find function "mutate"
```

```r
ggplot(actdataf, aes(x = interval, y = steps)) + geom_line(color = "green") +
        ggtitle("Mean Daily Steps Taken by Interval\nfor Weekdays & Weekends") +
        facet_grid(ddttype ~ .)
```

```
## Error in eval(expr, envir, enclos): could not find function "ggplot"
```

```r
last <- group_by(actdataf, ddttype)
```

```
## Error in eval(expr, envir, enclos): could not find function "group_by"
```

```r
summarize(last, mean=mean(steps, na.rm = TRUE))
```

```
## Error in eval(expr, envir, enclos): could not find function "summarize"
```
