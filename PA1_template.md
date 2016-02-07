# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data

ActivityDataSet <- read.csv("./repdata-data-activity/activity.csv");
ActivitySet <- group_by(ActivityDataSet, date) %>% summarise(sum(steps));



## What is mean total number of steps taken per day?

### Generating the Historgram of total stps per day
hist(ActivitySet$`sum(steps)`, breaks = 61, xlab = " total number of steps taken each day");

### Mean and Median of Total Steps
meanOfTotalStep <- mean(ActivitySet$`sum(steps)`, na.rm = T);
medianOfTotalStep <- median(ActivitySet$`sum(steps)`, na.rm = T);

## What is the average daily activity pattern?

### Make Time series plot
ActivitySet <- select(ActivityDataSet, date, interval, steps) %>%
                 group_by(interval) %>%
                 summarise(average = mean(steps, na.rm = T));
plot(ActivitySet$interval, ActivitySet$average, type = "l", 
     xlab = "5 minute interval", ylab = "the average number of steps taken, averaged across all days");
### Max average step in the interval of 4 minutes
MaxSteps <- select(ActivitySet, interval, average) %>% filter(average == max(average, na.rm = T))


## Imputing missing values

###Calculate the missing Value
MissingValue <- is.na(ActivityDataSet$steps)
table(MissingValue)

### Fill the missing values
fillFunction <- function(steps, intervals){
        fill <- NA;
        if (is.na(steps)) {
             fill <- (ActivitySet[ActivitySet$interval == intervals, "average"])
        }
        else (
                fill <- c(steps)
        )
        
        return (fill)
}

CompleteData <- ActivityDataSet
CompleteData$steps <- mapply(fillFunction, CompleteData$steps, CompleteData$interval)

### Generate Histogram of Complete Data set
CompleteData$steps <- as.numeric(CompleteData$steps)
CompleteActivitySet <- group_by(CompleteData, date) %>% summarise(sum(steps));
hist(CompleteActivitySet$`sum(steps)`, breaks =  61, xlab = " total number of steps taken each day");
meanOfTotalStepCom <- mean(CompleteActivitySet$`sum(steps)`, na.rm = T);
medianOfTotalStepCOm <- median(CompleteActivitySet$`sum(steps)`, na.rm = T);

## Are there differences in activity patterns between weekdays and weekends?

averages <- aggregate(steps ~ interval + day, data=CompleteData, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
        xlab("5-minute interval") + ylab("Number of steps")
