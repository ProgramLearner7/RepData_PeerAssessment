---
title: "Activity Monitor Data"
output: html_document
---

```{r package, echo=FALSE, message=FALSE}
library(ggplot2)
library(dplyr)
library(knitr)
```

Download images
```{r opts, echo = TRUE}
knitr::opts_chunk$set(
  fig.path = "images/"
)
```

**Read the activity data**
```{r ReadFile}
data = read.csv("activity.csv")
```

**Histogram of the total number of steps taken each day**
bin size is 30
```{r Histogram}
daily_step = data %>% group_by(date) %>% summarise(daily = sum(steps, na.rm = TRUE))
ggplot(daily_step, aes(x = daily)) + geom_histogram() +
        labs(title = "Number Daily Steps per Day", x = "Steps", y = "Frequency")
```

**Mean and median number of steps taken each day (including NAs)**
```{r Mean_Median}
mean = mean(daily_step$daily)
median = median(daily_step$daily)
```

* Mean of number of steps: `r mean`
* Median of number of steps: `r median`

**Time series plot of the average number of steps taken**
```{r Steps_Daily}
ggplot(daily_step, aes(x = as.Date(date), y = daily)) +
        geom_line() +
        labs(x = "Date", y = "Average Steps", title = "Average Number Of Steps Taken Across All Days")
```

**The 5-minute interval that, on average, contains the maximum number of steps**
```{r Max}
max = max(data$steps, na.rm = TRUE)
as.Date(subset(data, steps == max, select = date)$date)
```

**Code to describe and show a strategy for imputing missing data**
Fill the NA with the mean of that day (if there is no record for the entire day, the missing data will be 0)
```{r Imputation}
data_impute = data %>% group_by(date) %>% mutate(mean = mean(steps, na.rm = TRUE), total = sum(steps, na.rm = TRUE)) %>% 
        ungroup() %>% 
        mutate(steps = if_else(is.na(steps), 
                               if_else(is.nan(mean), 0, mean), as.numeric(steps)))
mean_impute = mean(data_impute$total)
median_impute = median(data_impute$total)
```

Activity with missing data | `r mean` | `r median`  
Activity with imputed mean data | `r mean_impute` | `r median_impute`

**Histogram of the total number of steps taken each day after missing values are imputed**
```{r Hist_Imputation}
data_impute %>% group_by(date) %>% summarise(total = sum(steps)) %>% 
        ggplot(aes(x = total)) +
        geom_histogram() +
        labs(title = "Number Daily Steps per Day_impute", x = "Steps", y = "Frequency")
```

**Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends**
* Removed missing data
```{r Week}
data_impute %>% 
        mutate(
                is_weekday = 
                       if_else(grepl("S(at|un)", weekdays(as.Date(date), abbr = TRUE)), "weekend",   "weekday")) %>% 
        ggplot(aes(x = interval, y = steps, group = is_weekday)) +
        geom_line(aes(color = is_weekday)) +
        facet_wrap(~ is_weekday, nrow = 2) +
          labs(title = "Difference Between Weekdays and Weekends", x = "Time Interval", y = "Average Number of Steps")
        
```

