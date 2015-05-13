setwd("C:/Users/Jesse/Desktop/Linux/R/ReproRes/Proj1")
if (!file.exists("activity.zip")) {
  download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", 
                destfile = "activity.zip")
}

if (!file.exists("activity.csv")) {
  unzip("activity.zip")
}

## Load data
fulldata <- read.csv("activity.csv")
fulldata$date <- as.Date(fulldata$date, "%Y-%m-%d")

## Calculate number of steps per day, removing NAs
nona <- fulldata[complete.cases(fulldata),]
daysteps <- aggregate(nona$steps, list(Date = nona$date), sum)

## Make histogram of distribution of steps per day
hist(daysteps$x,xlab="Steps",main = "Distribution of Total Steps per Day")

## Calculate mean and median numbers of steps per day
daymean <- mean(daysteps$x)
daymedian <- median(daysteps$x)

## Average each interval across all days
intsteps <- aggregate(nona$steps, list(Interval=nona$interval),mean)

## Plot time series of steps per interval vs. interval
plot(intsteps$Interval, intsteps$x, type = "l",xlab="Time (min)",ylab="Average Steps",
     main="Average Steps Throughout a Day")
maxint <- which.max(intsteps$x)

## Calculate total NAs in dataset
yesnas <- which(!complete.cases(fulldata))
length(yesnas)

## Fill in NAs with avg for that interval
fullerdata <- fulldata
for (i in 1:length(yesnas)) {
    place <- fulldata$interval[yesnas[i]]
    fullerdata$steps[yesnas[i]] <- intsteps$x[which(intsteps$Interval == place)]
}

## More complete histogram, mean, and median
daysteps2 <- aggregate(fullerdata$steps, list(Date = fullerdata$date), sum)
hist(daysteps2$x,xlab="Steps",main = "Distribution of Total Steps per Day")
daymean2 <- mean(daysteps2$x)
daymedian2 <- median(daysteps2$x)

## Add new variable to dataset - weekday or weekend
for (i in 1:length(fullerdata[,1])) {
    if (weekdays(fullerdata$date[i]) == "Monday" | weekdays(fullerdata$date[i]) == "Tuesday" |
            weekdays(fullerdata$date[i]) == "Wednesday" | weekdays(fullerdata$date[i]) == "Thursday" | 
            weekdays(fullerdata$date[i]) == "Friday") {
        fullerdata$weekday[i] <- "weekday"
    } else {
        fullerdata$weekday[i] <- "weekend"
    }
}

## Average each interval across all weekdays
weekdaydata <- subset(fullerdata, fullerdata$weekday == "weekday")
wkdayintsteps <- aggregate(weekdaydata$steps, list(Interval=weekdaydata$interval),mean)
wkdayintsteps$weekday <- "weekday"

## Average each interval across all weekends
weekenddata <- subset(fullerdata, fullerdata$weekday == "weekend")
wkendintsteps <- aggregate(weekenddata$steps, list(Interval=weekenddata$interval),mean)
wkendintsteps$weekday <- "weekend"

## Combine datasets
byday <- wkdayintsteps
byday[289:576,] <- wkendintsteps

## Create dual plot of avg weekdays vs. weekends
library(lattice)
xyplot(x ~ Interval | weekday, data = byday, layout = c(1,2), type = "l", ylab="Average Steps Taken", xlab = "Time (min)")
