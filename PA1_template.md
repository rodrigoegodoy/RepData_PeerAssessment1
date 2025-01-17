---
title: "PA1_template"
author: "Rodrigo Espindola Godoy"
date: "14/04/2021"
output: 
  html_document: 
    keep_md: yes
---


```r
knitr::opts_chunk$set(echo = TRUE, fig.path = "figures/", include=TRUE)
```

First load the packages needed to do the analysis

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```
## Warning: package 'ggplot2' was built under R version 4.0.4
```

Download, unzip  and read the file

```r
zip <- "activity.zip"

if (!file.exists(zip)){
    fileurl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(fileurl, zip, method = "curl")
}        

if(!file.exists("activity")) {
    unzip(zip)
} 

## Read the file
activity <- read.csv("activity.csv")
```

## What is mean total number of steps taken per day?


```r
## Change the date variable to a "Date" class
activity$date <- as.Date(as.character(activity$date, "%Y-%m-%d"))
## Sum the number of steps taken by day
stepsday <- tapply(activity$steps, activity$date, sum)
## Plot the histogram showing the number of steps taken each day
hist(stepsday, xlab = "Steps by day", ylab = "Number of days",
     main = "Frequency of steps by day")
```

![](figures/stepsperday-1.png)<!-- -->

```r
## Calculate the mean
mean(stepsday, na.rm = TRUE)
```

```
## [1] 10766.19
```

```r
## Calculate the median
median(stepsday, na.rm = TRUE)
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
## Group the data by the interval and take the mean steps taken in all days
dailyactivity <- activity %>% 
    group_by(interval) %>%
        summarise(steps = mean(steps, na.rm = TRUE))
## Plot a line graphic showing the average steps taken in all days by the interval
da <- ggplot(dailyactivity, aes(x = interval, steps))
    da+geom_line() +
    labs(x  = "Interval", y = "Average steps in all days", 
         title = "Average steps in all days by Interval") +
    theme(plot.title = element_text(hjust = 0.5))
```

![](figures/averagedailyactivity-1.png)<!-- -->

```r
## Which 5-minute interval, contains the maximum number of steps? 
maxsteps <- dailyactivity %>% 
                arrange(desc(steps))
maxsteps[1,]
```

```
## # A tibble: 1 x 2
##   interval steps
##      <int> <dbl>
## 1      835  206.
```
## Imputing missing values

```r
## Calculate and report the total number of missing values in the dataset
sum(is.na(activity))
```

```
## [1] 2304
```

```r
## Devise a strategy for filling in all of the missing values in the dataset. Create a new dataset that is equal to the original dataset but with the missing data filled in.
activity2 <- merge(activity, dailyactivity, by = "interval") %>% 
    mutate(steps = coalesce(steps.x, steps.y)) %>%
        select(-c(steps.x, steps.y))

## Make a histogram of the total number of steps taken each day 
stepsdaynona <- tapply(activity2$steps, activity2$date, sum)
hist(stepsdaynona, xlab = "Steps by day", ylab = "Number of days",
     main = "Frequency of steps by day")
```

![](figures/missingvalues-1.png)<!-- -->

```r
## Calculate and report the mean and median total number of steps taken per day
mean(stepsdaynona)
```

```
## [1] 10766.19
```

```r
median(stepsdaynona)
```

```
## [1] 10766.19
```
As shown in the results the only difference observed between the data with and without the NAs is the median, that is higher in the analysis without the NAs. 

## Are there differences in activity patterns between weekdays and weekends?

```r
## Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
week <- activity2 %>%  
    mutate(weekday = case_when(weekdays(date) =="sábado" ~ "Weekend", 
                               weekdays(date) == "domingo" ~ "Weekend",
                               TRUE ~ "Weekday")) %>%
                group_by(interval, weekday) %>%
                    summarise(steps = mean(steps), .groups = 'drop_last')
## Make a plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days
wks <- ggplot(data = week, aes(x = interval, y = steps))
    wks+geom_line() +
    facet_grid(vars(weekday)) +
    labs(x = "Interval", y = "Average steps taken", 
         title = "Average steps taken by 5 minutes Interval period") +
            theme(plot.title = element_text(hjust = 0.5))
```

![](figures/weekdays-1.png)<!-- -->
