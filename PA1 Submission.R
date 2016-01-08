## Loading and preprocessing the data
if(!file.exists("./data"))
{
  dir.create("./data")
  fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file(fileUrl, destfile="./data/Activity.zip")
  unzip("./data/Activity.zip", exdir = "Project1")
}

activity <- read.csv("./Project1/Activity.csv", header=T)

## What is mean total number of steps taken per day?
act_day <- aggregate(activity$steps, by = list(activity$date), FUN=sum)
colnames(act_day) <- c("Date", "Steps")
hist(act_day$Steps, main="Steps Taken Per Day", col="blue")
mean(act_day$Steps, na.rm=T)
median(act_day$Steps, na.rm=T)

## What is the average daily activity pattern?
act_int <- aggregate(activity$steps, by=list(activity$interval), FUN=sum, na.rm=T)
colnames(act_int) <- c("Interval", "Steps")
with(act_int, plot(Steps~Interval, type="l", lwd=3, col="green", main="Steps Taken by Interval"))
act_int[which.max(act_int$Steps),]$Interval

## Imputing missing values using mean of 5-minute interval
sum(is.na(activity$steps))

act_int_mean <- function(x) {
  avg <- aggregate(activity$steps, by=list(activity$interval), FUN=mean, na.rm=T)
  avg[is.na(avg)] <- 0
  colnames(avg) <- c("Interval", "Avg_Steps")
  avg[which(avg$Interval == x),]$Avg_Steps
}

activity_no_NA <- activity

for(i in 1:length(activity_no_NA$steps)){
  if(is.na(activity_no_NA$steps[i])){
    activity_no_NA$steps[i] <- act_int_mean(activity_no_NA$interval[i])
  } 
}

hist(activity_no_NA$steps)
mean(activity_no_NA$steps)
median(activity_no_NA$steps)

## Are there differences in activity patterns between weekdays and weekends?
for(i in 1:length(activity_no_NA$date)){
  if(weekdays(as.Date(activity_no_NA$date[i])) == "Saturday"){
    activity_no_NA$weekday[i] <- "weekend"
  }
  else if(weekdays(as.Date(activity_no_NA$date[i])) == "Sunday"){
    activity_no_NA$weekday[i] <- "weekend"
  }
  else{
    activity_no_NA$weekday[i] <- "weekday"
  }
}

activity_no_NA$weekday <- as.factor(activity_no_NA$weekday)

library(ggplot2)

act_int_avg <- with(activity_no_NA, aggregate(steps, by=list(interval, weekday), FUN=mean))
colnames(act_int_avg) <- c("interval", "weekday", "average")

ggplot(act_int_avg, aes(x=interval, y=average)) + 
  facet_wrap(~weekday, ncol=1, as.table=FALSE) + 
  geom_line(lwd=1, color="green") +
  ggtitle("Average Number of Steps by Interval") +
  theme(plot.title=element_text(size=20, face="bold", vjust=2))