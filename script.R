## Part I - Loading and preprocessing the data

# Unziping file
unzip("activity.zip")

# Loading data into a  file
active <- read.csv("actvity.csv", header=TRUE, sep=",")

# Converting date variable into an date format
active$date <- as.Date(active$date)

# presenting and summaring data
head(active)
summary(active)

# checkig for how many missing values is in the steps variable
missing <- sum(is.na(active$steps))

## Part II - What is mean total number of steps taken per day
## for this part of the assignment the missing values are ignored

# making a histogram of the total number of steps taken each day
# summing the steps by day
sumByDay <- tapply(active$steps, active$date, sum)

# making a histogram
hist(sumByDay, main="total number of steps taken each day",
     xlab="steps each day")

# calculating the mean and median total number of steps taken per day
# using summary function on the sumByDay variable
summary <- summary(sumByDay)

# presenting the median value
summary[3]

# presenting the mean value
summary[4]

## Part III - what is the average daily activity pattern ?

# averaging the steps by interval
aveByInter <- tapply(active$steps, active$interval, mean, na.rm=TRUE)

# making the plot
plot(aveByInter, type="l", main="Average number of steps by 5 min interval",
     ylab="averaged number of steps", xlab="time of day", xaxt='n')
axis(1, at=c(0,48,96,144,192, 240, 288), labels=c("0", "4", "8", "12", "16",
                                             "20", "24"))

# which 5-minute interval, on average across all the days in the dataset
# contains the maximum number of steps

aveByInter[which.max(aveByInter)]

## Part IV - Input missing values

# calculating and printing  the total number of missing values in the dataset
missing <- sum(is.na(active))
print(missing)

# missing values will be filled by the mean for particular 5-minute interval
# for this we will use the aveByInter variable
aveByInter <- as.data.frame(aveByInter)
for(i in 1:length(active$steps)) {
    if(is.na(active$steps[i])) {
        active$steps[i] = as.numeric(aveByInter[i,])
    }
}


# making a histogram of the total number of steps taken each day after after
# filling the missing values
# summing steps by day
sumByDayFull <- tapply(active$steps, active$date, sum)

# making a histogram
hist(sumByDayFull, main="total number of days taken each day (filled NA's)",
     xlab="steps each day")

# calculating the mean and median total number of steps taken per day
# with filled missing values
# using summary function on the sumByDay variable
summaryFull <- summary(sumByDayFull)

# presenting the Median value
summaryFull[3]

# presenting the Mean value
summaryFull[4]

## PART V - Are there difference in activity patterns between weekdays and
## weekends

# creating a new factor variable in the dataset with two levels 1) weekday
# and 2) weekend indicating whether a given day is a weekday or weekend day
weekend <- weekdays(active$date) == "Saturday"|
    weekdays(active$date) == "Sunday"
active$dayType[weekend] <- "weekend"
active$dayType[!weekend] <- "weekday"
active$day <- as.factor(active$day)

# making a panel plot containing a time series plot
library(lattice)

aveStepInt <- aggregate(active$steps, by=list(active$interval, active$dayType),
                        mean, na.rm=TRUE)
names(aveStepInt) <- c("interval", "dayType", "steps")

xyplot(steps ~ interval | dayType, data = aveStepInt, type="l", layout=c(1,2))




