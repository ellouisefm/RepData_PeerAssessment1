library(dplyr)
library(knitr)
library(ggplot2)
library(timeDate)

### using URL
# fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
# download.file(fileUrl, destfile = "./Dataset.zip")
# unzip("./Dataset.zip")

## Loading and preprocessing the data
unzip("./activity.zip")
activity <- read.csv("./activity.csv", header = TRUE, sep = ",")
# convert character date to Date type
activity$date <- as.Date(activity$date)
# remove NA values in the steps
fltr_activity <- filter(activity, !is.na(activity$steps))
# show more precision in tibble
options(pillar.sigfig = 7)

## What is mean total number of steps taken per day?
# calculate total sum per day
steps_per_day <- fltr_activity %>%
                  group_by(date) %>%
                  summarise(steps = sum(steps))  
# plot histogram of daily steps
png("Total Number of Steps Taken Each Day.png", width=600, height=400)
ggplot(steps_per_day, aes(x = steps)) +
  geom_histogram(fill = "purple", binwidth = 1000) +
  labs(title = "Total Number of Steps Taken Each Day", x = "Total Steps", y = "Frequency")
dev.off()
# mean and median
steps_per_day %>% summarise(mean_steps = mean(steps, na.rm = TRUE), 
                            median_steps = median(steps, na.rm = TRUE))

## What is the average daily activity pattern?
# calculate daily average steps by interval
daily_pattern <- fltr_activity %>%
                  group_by(interval) %>%
                  summarise(AverageStep = mean(steps, na.rm = TRUE))
# plot average daily step
png("Average Daily Pattern.png", width=600, height=400)
ggplot(data = daily_pattern, aes(x = interval, y = AverageStep)) + 
  geom_line() +
  labs(x = "Minute Interval", y = "Average Steps per Day") + 
  ggtitle("Average Daily Pattern") + 
  geom_line(color="purple", size=1.15)
dev.off()
# interval with maximum steps
filter(daily_pattern, AverageStep == max(AverageStep))

## Imputing missing values
# total number of rows with NA
sum(is.na(activity))
# impute missing values
imp_activity <- activity
imp_activity[is.na(imp_activity$steps), "steps"] <- imp_activity$steps %>% 
                                                      median(na.rm = T)
# calculate total sum per day
imp_steps_per_day <- imp_activity %>%
                      group_by(date) %>%
                      summarise(steps = sum(steps))  
# plot histogram of daily steps
png("Total Number of Steps Taken Each Day - Imputed Data.png", width=600, height=400)
ggplot(imp_steps_per_day, aes(x = steps)) +
  geom_histogram(fill = "cyan", binwidth = 1000) +
  labs(title = "Total Number of Steps Taken Each Day", x = "Total Steps", y = "Frequency")
dev.off()
# mean and median
imp_steps_per_day %>% summarise(mean_steps = mean(steps, na.rm = TRUE), 
                            median_steps = median(steps, na.rm = TRUE))
## insert table here to compare values

## Are there differences in activity patterns between weekdays and weekends?
imp_activity$day <- ifelse(isWeekday(imp_activity$date) == TRUE, "Weekday", "Weekend")
wday_wend_interval <- aggregate(steps~interval + day, imp_activity, mean, na.rm = TRUE)
png("Activity Pattern Between Weekday and Weekend.png", width=720, height=600)
ggplot(wday_wend_interval, aes(x = interval, y = steps, color = day)) + 
  geom_line(size=1.15) + 
  facet_wrap(~day, ncol = 1, nrow = 2) + 
  labs(title = "Activity Pattern Between Weekday and Weekend", x = "Interval", y = "Average number of steps")
dev.off()