---
title: "Reproducible Research: Peer Assessment 1"
output: 
    html_document:
        keep_md: true
---




## Loading and preprocessing the data



```r
# require libraries for dplyr, ggplot2, etc.
require(tidyverse)
# require knitr for making tables with kable
require(knitr)
# create a temporary file to handle the zipped folder
temp <- tempfile()
# download the the zipped folder to the temp file
download.file('https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip',temp)
# read the csv file that is in the temp file to a tibble
df <- read_csv(unz(temp, 'activity.csv'))
# delete the temporary file
unlink(temp)
```


## What is mean total number of steps taken per day?


#### Calculate the total number of steps taken per day

```r
# group by date and take the sum of steps for each date in a new tibble
daily_steps_df <- df %>% na.omit() %>% 
                         group_by(date) %>% 
                         summarize(total_steps = sum(steps))
kable(daily_steps_df)
```



date          total_steps
-----------  ------------
2012-10-02            126
2012-10-03          11352
2012-10-04          12116
2012-10-05          13294
2012-10-06          15420
2012-10-07          11015
2012-10-09          12811
2012-10-10           9900
2012-10-11          10304
2012-10-12          17382
2012-10-13          12426
2012-10-14          15098
2012-10-15          10139
2012-10-16          15084
2012-10-17          13452
2012-10-18          10056
2012-10-19          11829
2012-10-20          10395
2012-10-21           8821
2012-10-22          13460
2012-10-23           8918
2012-10-24           8355
2012-10-25           2492
2012-10-26           6778
2012-10-27          10119
2012-10-28          11458
2012-10-29           5018
2012-10-30           9819
2012-10-31          15414
2012-11-02          10600
2012-11-03          10571
2012-11-05          10439
2012-11-06           8334
2012-11-07          12883
2012-11-08           3219
2012-11-11          12608
2012-11-12          10765
2012-11-13           7336
2012-11-15             41
2012-11-16           5441
2012-11-17          14339
2012-11-18          15110
2012-11-19           8841
2012-11-20           4472
2012-11-21          12787
2012-11-22          20427
2012-11-23          21194
2012-11-24          14478
2012-11-25          11834
2012-11-26          11162
2012-11-27          13646
2012-11-28          10183
2012-11-29           7047

#### Make a histogram of the total number of steps taken each day

```r
# plot a histogram of daily total steps
ggplot(daily_steps_df, aes(x=total_steps)) + geom_histogram()
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

#### Calculate and report the mean and median of the total number of steps taken per day

```r
# calculate and report the mean of total daily steps
print(paste('Mean of daily total steps:',mean(daily_steps_df$total_steps)))
```

```
## [1] "Mean of daily total steps: 10766.1886792453"
```

```r
# calculate and report the median of total daily steps
print(paste('Median of daily total steps:',median(daily_steps_df$total_steps)))
```

```
## [1] "Median of daily total steps: 10765"
```


## What is the average daily activity pattern?


#### Make a time series plot of the 5-minute interval and the average number of steps taken, averaged across all days

```r
# group by interval and take the average of steps for each interval in a 
# new tibble
interval_steps_df <- df %>% na.omit() %>% 
                         group_by(interval) %>% 
                         summarize(average_steps = mean(steps))
# plot a time series of average steps by interval
ggplot(interval_steps_df, aes(x=interval,y=average_steps)) + geom_line()
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

#### Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
# calculate and report the interval with the maximum average steps
kable(interval_steps_df[which.max(interval_steps_df$average_steps),])
```



 interval   average_steps
---------  --------------
      835        206.1698


## Imputing missing values


#### Calculate and report the total number of missing values in the dataset

```r
# calculate and report the number of missing values
print(paste('Total NA values:',sum(is.na(df$steps))))
```

```
## [1] "Total NA values: 2304"
```

#### Devise a strategy for filling in all of the missing values in the dataset.

**Strategy for imputing missing values:**

1. Separate NA values
2. Add average steps for that interval to each
3. Recombine previously NA values with the original dataset

#### Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
# separate NA values
na_df <- df %>% filter(is.na(steps))
# add average steps for that interval to each by merging with the previously
# created dataframe containing the average for each interval
imputed_df <- na_df %>% merge(interval_steps_df) %>% 
                        select(-steps) %>%
                        select(steps=average_steps,date,interval)
# recombine with the original dataframe after first removing the NA values
comb_df <- df %>% na.omit() %>%
                  rbind(imputed_df)
# check that there are no NA's
print(paste('Number of NAs in new dataset:',sum(is.na(comb_df$steps))))
```

```
## [1] "Number of NAs in new dataset: 0"
```

```r
# check that we didn't lose any data points
print(paste('Original dataset rows:',nrow(df)))
```

```
## [1] "Original dataset rows: 17568"
```

```r
print(paste('New dataset rows:',nrow(comb_df)))
```

```
## [1] "New dataset rows: 17568"
```

#### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day

```r
# reuse previous code for generating a histogram and calculating/reporting
# mean and median steps per day using the newly imputed dataframe

# group by date and take the sum of steps for each date in a new tibble
imp_daily_steps_df <- comb_df %>% group_by(date) %>% 
                                  summarize(total_steps = sum(steps))
# plot a histogram of daily total steps
ggplot(imp_daily_steps_df, aes(x=total_steps)) + geom_histogram()
```

![](PA1_template_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

```r
# calculate and report the mean of total daily steps for original/imputed
print(paste("Original daily mean steps:",mean(daily_steps_df$total_steps)))
```

```
## [1] "Original daily mean steps: 10766.1886792453"
```

```r
print(paste("Imputed daily mean steps:",mean(imp_daily_steps_df$total_steps)))
```

```
## [1] "Imputed daily mean steps: 10766.1886792453"
```

```r
# calculate and report the median of total daily steps for original/imputed
print(paste("Original daily median steps:",median(daily_steps_df$total_steps)))
```

```
## [1] "Original daily median steps: 10765"
```

```r
print(paste("Imputed daily median steps:",median(imp_daily_steps_df$total_steps)))
```

```
## [1] "Imputed daily median steps: 10766.1886792453"
```

#### Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
# calculate and report the mean of total daily steps for original/imputed
print(paste("Original daily mean steps:",mean(daily_steps_df$total_steps)))
```

```
## [1] "Original daily mean steps: 10766.1886792453"
```

```r
print(paste("Imputed daily mean steps:",mean(imp_daily_steps_df$total_steps)))
```

```
## [1] "Imputed daily mean steps: 10766.1886792453"
```

```r
# calculate and report the median of total daily steps for original/imputed
print(paste("Original daily median steps:",median(daily_steps_df$total_steps)))
```

```
## [1] "Original daily median steps: 10765"
```

```r
print(paste("Imputed daily median steps:",median(imp_daily_steps_df$total_steps)))
```

```
## [1] "Imputed daily median steps: 10766.1886792453"
```


## Are there differences in activity patterns between weekdays and weekends?


#### Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day

```r
# add a new column with values based on a conditional that tests for weekend
# or weekday
week_df <- comb_df %>% mutate(day_type = 
                                  if_else(weekdays(date,abbr=TRUE)
                                          %in% c('Sat','Sun'),
                                          'weekend','weekday'))
print(head(week_df))
```

```
## # A tibble: 6 x 4
##   steps date       interval day_type
##   <dbl> <date>        <dbl> <chr>   
## 1     0 2012-10-02        0 weekday 
## 2     0 2012-10-02        5 weekday 
## 3     0 2012-10-02       10 weekday 
## 4     0 2012-10-02       15 weekday 
## 5     0 2012-10-02       20 weekday 
## 6     0 2012-10-02       25 weekday
```

#### Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken, averaged across all weekday days or weekend days

```r
# group by interval and type of day, then take the average of steps for each 
# interval in a new tibble
week_interval_df <- week_df %>% na.omit() %>% 
                    group_by(interval,day_type) %>% 
                    summarize(average_steps = mean(steps))
# plot a time series of average steps by interval, separating facets by type
# of day
ggplot(week_interval_df, aes(x=interval,y=average_steps)) + 
    geom_line() + 
    facet_grid(day_type ~ .)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
