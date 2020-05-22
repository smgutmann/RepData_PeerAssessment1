---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

Checking R version and loading required libraries:


```r
R.version
```

```
##                _                           
## platform       x86_64-pc-linux-gnu         
## arch           x86_64                      
## os             linux-gnu                   
## system         x86_64, linux-gnu           
## status                                     
## major          4                           
## minor          0.0                         
## year           2020                        
## month          04                          
## day            24                          
## svn rev        78286                       
## language       R                           
## version.string R version 4.0.0 (2020-04-24)
## nickname       Arbor Day
```

```r
library(dplyr)
```

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

```r
library(ggplot2)
```

Unzip the file and read in the data:


```r
unzip("activity.zip")
amd1 <- read.csv("activity.csv")
```

Check amd1 data frame and convert date from character to date values


```r
class(amd1$date)
```

```
## [1] "character"
```

```r
amd1$date <- as.Date(amd1$date, "%Y-%m-%d")
class(amd1$date)
```

```
## [1] "Date"
```



## What is mean total number of steps taken per day?

**Calculate the total number of steps taken per day**


```r
daily_amd1 <- amd1 %>% group_by(date) %>% summarise(total_steps = sum(steps)) %>% na.omit()
```

**Make a histogram of the total number of steps taken per day**


```r
hist(daily_amd1$total_steps, col = "lightblue", main = "Histogram of total steps taken per day", xlab = "Sum of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

**Calculate and report the mean and median of the total number of steps taken per day**


```r
summary(daily_amd1$total_steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10765   10766   13294   21194
```

Mean of the total number of steps: 10766  
Median of the total number of steps: 10765  

## What is the average daily activity pattern?

**Make a time series plot of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)**


```r
# Ignoring NAs

meanSteps <- amd1 %>% group_by(interval) %>% summarise(mean_steps = mean(steps, na.rm = TRUE))

# creating the plot
ggplot(meanSteps, aes(x = interval, y = mean_steps)) + 
        geom_line() + 
        labs(x = "5 min intervals", y = "Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png)<!-- -->


**Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?**


```r
# Find 5-minute interval associated with maximum number of steps

maxStepInt <- meanSteps[which.max(meanSteps$mean_steps), ]
```

The interval that contains the maximum numbers of steps is


```r
print(as.character(maxStepInt[, 1]))
```

```
## [1] "835"
```

## Imputing missing values

**Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)**

The number of NA's was calculated as follows:


```r
summary(amd1)
```

```
##      steps             date               interval     
##  Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
##  1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
##  Median :  0.00   Median :2012-10-31   Median :1177.5  
##  Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
##  3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
##  Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
##  NA's   :2304
```


From this output we get that there are 2304 NAs and that they are all located in the "steps" column. This number could also be derived from


```r
sum(is.na(amd1))
```

```
## [1] 2304
```

which corresponds to about 4 % of the total number of values in the dataset


```r
mean(is.na(amd1))
```

```
## [1] 0.04371585
```


**Devise a strategy for filling in all of the missing values in the dataset.** 

The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.

- As mentionend above, the "steps" column is the only column that contains NAs. SO we focus on replacing the NAs in amd1$steps  
- Conditional element selection is carried out using ifelse  
- If is.na(amd1$steps) is true it will be replaced by the mean number of steps per interval. If not the original value will be used  


**Create a new dataset that is equal to the original dataset but with the missing data filled in**


```r
amd2 <- amd1 %>% group_by(interval) %>% mutate(steps = ifelse(is.na(steps), mean(steps, na.rm = TRUE), steps))
```

**Make a histogram of the total number of steps taken each day and calculate and report the mean and median total number of steps taken per day.** Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

Calculate total steps made per day using the new dataset


```r
# New dataframe with total steps per day
daily_amd2 <- amd2 %>% group_by(date) %>% summarise(total_steps = sum(steps))
```

**Plot a histogram of the total number of steps taken each day:**


```r
hist(daily_amd2$total_steps, col = "lightblue", main = "Histogram of total steps taken per day", xlab = "Sum of Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-15-1.png)<!-- -->

**Calculate mean and median total number of steps taken per day:**


```r
summary(daily_amd2$total_steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10766   10766   12811   21194
```

The median (10766 steps) and mean (10766 steps) values of the dataset with imputed values are similar to the values reported above. 

## Are there differences in activity patterns between weekdays and weekends?

Define weekdays and weekends in dataset:


```r
tmp1 <- amd1 %>% mutate(weekend = (weekdays(date) == "Saturday" | weekdays(date) == "Sunday"))
tmp1 <- tmp1 %>% mutate(weekend = ifelse(weekend == "TRUE", "Weekend", "Weekday"))
```


Calculate average number of steps taken per day for each interval and weekday or weekend:


```r
tmp2 <- tmp1 %>% group_by(interval, weekend) %>% summarise(mean_steps = mean(steps, na.rm = TRUE))
```

Plot 5 min intervals vs. average number of steps for weekends and weekdays:


```r
ggplot(tmp2, aes(interval, mean_steps)) + 
        geom_line() + facet_grid(~weekend) + 
        labs(x = "5 min intervals", y = "Average number of steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-19-1.png)<!-- -->


```r
wd <- filter(tmp2, weekend == "Weekday")
summary(wd$mean_steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   2.218  23.974  35.338  51.872 234.103
```

```r
we <- filter(tmp2, weekend == "Weekend")
summary(we$mean_steps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##   0.000   1.107  32.036  43.078  75.571 175.000
```

Mean and median steps are higher during the weekends than on weekdays. 

