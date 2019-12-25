
# Reproducible Research: Peer Assessment 1

This is an R Markdown document for peer assessment 1 of Coursera's Reproducible Research course.

Zara Zaffar, 2019

## Loading and preprocessing the data
```{r, echo=TRUE}
library("data.table")
library(httpuv)
library(dplyr)
library(ggplot2)
library(lubridate)
```

Reading csv Data into Data.Table. 
```{r, echo=TRUE}
getwd()
download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "activity")
unzip("activity")
unlink("activity")

#Load the data
activity <- read.csv("activity.csv")
```
We can then inspect the dataset

```{r, echo=TRUE}
summary(activity)

```
```{r, echo=TRUE}
str(activity)
```

Tidying the data
```{r, echo=TRUE}
activity$date <- ymd(activity$date)
```
Removing rows with NA
```{r, echo=TRUE}
activity1 <- na.omit(activity)
```
## What is mean total number of steps taken per day?
1. Histogram of the total number of steps taken each day

Summarize data for ggplot
```{r, echo=TRUE}
activity2 <- summarize(group_by(activity1,date),daily.step=sum(steps))
mean.activity <- as.integer(mean(activity2$daily.step))
median.activity <- as.integer(median(activity2$daily.step))

```
Plot histogram

```{r, echo=TRUE}
plot.steps.day <- ggplot(activity2, aes(x=daily.step)) + 
  geom_histogram(binwidth = 1000, aes(y=..count.., fill=..count..)) + 
  geom_vline(xintercept=mean.activity, colour="red", linetype="dashed", size=1) +
  geom_vline(xintercept=median.activity, colour="green" , linetype="dotted", size=1) +
  labs(title="Histogram of Number of Steps taken each day", y="Frequency", x="Daily Steps") 
plot.steps.day
```

2. Calculating Mean and Median of the total number of steps taken each day

```{r, echo=TRUE}
# Mean total number of steps taken each day
mean.activity
```

```{r, echo=TRUE}
# Median total number of steps taken each day
median.activity
```

Mean steps per day : 10766  
Median steps per day : 10765  

## What is the average daily activity pattern?

1. Make a time series plot (i.e.type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```{r, echo=TRUE}

# Prepare data for ggplot

activity3 <- activity1 %>% group_by(interval) %>% summarize(mean.step=mean(steps))

# Plot average number of steps by 5-minute interval

plot.step.interval <- ggplot(activity3, aes(x=interval,y=mean.step)) + 
  geom_line(color="red") + 
  labs(title="Average Number of Steps Taken vs 5-min Interval", y="Average Number of Steps", x="5-min Interval Times Series")

plot.step.interval

```

2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```{r, echo=TRUE}
optimal <- which.max(activity3$mean.step)
optimal.step <- activity3$interval[optimal]
sprintf("Maximum number of steps is coming from %gth 5-min interval", optimal.step)
```

##Imputing missing values

1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with \color{red}{\verb|NA|}NAs)
```{r, echo=TRUE}
#Total number of missing values in the dataset
sum(is.na(activity))

```
Total number of missing values in the dataset is 2304.

2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.
```{r, echo=TRUE}
impute.activity <- activity
impute.activity$steps[is.na(impute.activity$steps)] <- mean(impute.activity$steps,na.rm=TRUE)
impute.activity$steps <- as.numeric(impute.activity$steps)
impute.activity$interval <- as.numeric(impute.activity$interval)
colSums(is.na(impute.activity))

```


```{r, echo=TRUE}
# Subsetting general dataset with missing values only
missing_values <- subset(activity, is.na(steps))
# Plot repartition, by interval and by date 
par(mfrow = c(2,1), mar = c(2, 2, 1, 1))
hist(missing_values$interval, main="NAs repartition per interval")
hist(as.numeric(missing_values$date), main = "NAs repartion per date", breaks = 61)
```

We see that NAs run equally over all intervals. On the other hand, checking with
dates, we see all NA's are spread between 8 specific days only. To reduce that effect, best will be to take the mean for missing interval across all the days in the dataset.

3.Create a new dataset that is equal to the original dataset but with the missing data filled in.
```{r, echo=TRUE}
summary(missing_values)
```
4.Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?
```{r, echo=TRUE}
# Summarize data by date

impute.activity2 <- summarize(group_by(impute.activity,date),daily.step=sum(steps))
mean.impute <- as.integer(mean(impute.activity2$daily.step))
mean.impute
```

```{r, echo=TRUE}
median.impute <- as.integer(median(impute.activity2$daily.step))
median.impute
```

```{r, echo=TRUE}
# Plot histogram
plot.steps.day <- ggplot(impute.activity2, aes(x=daily.step)) + 
  geom_histogram(binwidth = 1000, aes(y=..count.., fill=..count..)) + 
  geom_vline(xintercept=mean.impute, colour="red", linetype="dashed", size=1) +
  geom_vline(xintercept=median.impute, colour="green" , linetype="dotted", size=1) +
  labs(title="Histogram of Number of Steps taken each day (impute)", y="Frequency", x="Daily Steps")
plot.steps.day
```

Mean total number of steps taken each day(after impute) is 10766.
Median total number of steps taken each day(after impute) is 10766.

After imputation Mean and Median results same values, however histogram looks more distributed.

## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() will come handy. Use the dataset with the filled-in missing values for this part.

1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.
```{r, echo=TRUE}
impute.activity$day <- ifelse(weekdays(impute.activity$date) %in% c("Saturday","Sunday"), "weekday", "weekend")
```
2. Make a panel plot containing a time series plot (i.e. \color{red}{\verb|type = "l"|}type="l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.
```{r, echo=TRUE}
# Preparing data for ggplot
impute.df <- impute.activity %>% group_by(interval,day) %>% summarise(mean.step=mean(steps))

# Plot Average steps across weekday/weekend vs 5-min interval Time Series
plot.weekday.interval <- ggplot(impute.df, aes(x=interval, y=mean.step, color=day)) + 
  facet_grid(day~.) +
  geom_line() + 
  labs(title="Average Number of Steps Taken vs 5-min Interval on Weekday/Weekend", y="Average Number of Steps", x="5-min Interval Times Series")
plot.weekday.interval
```


