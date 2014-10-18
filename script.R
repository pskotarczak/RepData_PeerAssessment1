## Part I - Loading and preprocessing the data

# Unzipung file
unzip("activity.zip")

# Loading data into a  file
active <- read.csv("actvity.csv", header=TRUE, sep=",")

# Converting date variable into an date format
active$date <- as.Data(active$date)

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

# presenting the mean value
summary[3]

# presenting the median value
summary[4]

## Part III - what is the average daily activity pattern ?

# averaging the steps by interval
aveByInter <- tapply(active$steps, active$interval, mean, na.rm=TRUE)

# making the plot
plot(aveByInter, type="l", main="Average number of steps by 5 min interval",
     ylab="averaged number of steps", xlab="time of day")
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
        active$steps[i] = aveByInter[i]
    }
}

