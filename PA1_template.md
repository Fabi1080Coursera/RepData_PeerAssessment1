# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the steps
Unzip the steps file and read it in the same line

```r
steps <- read.csv(unz("activity.zip", "activity.csv"), header = TRUE)
```

Convert the *Date* and *Interval* variable into a datetime variable:  

- Add leading zeros to *Interval* 


```r
steps$interval <- formatC(steps$interval, width = 4, format = "d", flag = "0")
head(steps$interval, 20)
```

```
##  [1] "0000" "0005" "0010" "0015" "0020" "0025" "0030" "0035" "0040" "0045"
## [11] "0050" "0055" "0100" "0105" "0110" "0115" "0120" "0125" "0130" "0135"
```

- paste the date and interval to one string and parse it with the lubridate function. Save the result in a new variable


```r
library(dplyr)
library(lubridate)
```

```r
steps <- steps %>%
  mutate(date = ymd(date), datetime = parse_date_time(paste(date, interval, sep = " "), orders = "ymd hm"))
head(steps$datetime, 20)
```

```
##  [1] "2012-10-01 00:00:00 UTC" "2012-10-01 00:05:00 UTC"
##  [3] "2012-10-01 00:10:00 UTC" "2012-10-01 00:15:00 UTC"
##  [5] "2012-10-01 00:20:00 UTC" "2012-10-01 00:25:00 UTC"
##  [7] "2012-10-01 00:30:00 UTC" "2012-10-01 00:35:00 UTC"
##  [9] "2012-10-01 00:40:00 UTC" "2012-10-01 00:45:00 UTC"
## [11] "2012-10-01 00:50:00 UTC" "2012-10-01 00:55:00 UTC"
## [13] "2012-10-01 01:00:00 UTC" "2012-10-01 01:05:00 UTC"
## [15] "2012-10-01 01:10:00 UTC" "2012-10-01 01:15:00 UTC"
## [17] "2012-10-01 01:20:00 UTC" "2012-10-01 01:25:00 UTC"
## [19] "2012-10-01 01:30:00 UTC" "2012-10-01 01:35:00 UTC"
```

## What is mean total number of steps taken per day?
Calculate the sum of steps per day and the overall mean and median

```r
stepsperday <- steps %>%
  group_by(date) %>%
  summarise(total_steps = sum(steps))

mean_steps_per_day <- mean(stepsperday$total_steps, na.rm = TRUE)
median_steps_per_day <- median(stepsperday$total_steps, na.rm = TRUE)
```
Plot the steps per day

```r
library(ggplot2)
```

```r
qplot(stepsperday$total_steps, geom = "histogram", main = "Histogram of the total steps made each day", xlab = "Steps", binwidth = 2000) +
  geom_vline(aes(xintercept=mean_steps_per_day, color = "Mean"), show.legend = TRUE) +
  geom_vline(aes(xintercept=median_steps_per_day, color = "Median"), show.legend = TRUE)
```

```
## Warning: Removed 8 rows containing non-finite values (stat_bin).
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->



## What is the average daily activity pattern?



## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
