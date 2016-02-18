### this program analyzes daily stats of data from tracking devices such as
# fitbit, jowebone, and etc

# read data
rawData <- read.csv("activity.csv");

# data preprocessing 
rawData$date <- as.POSIXlt(rawData$date);
DF2 <- data.frame(rawData, Day = as.Date(rawData$date));
good <- !is.na(DF2[, 1]);
DF2good <- DF2[good, ];

# Calculate and report the total number of missing values in the dataset
# (i.e. the total number of rows with 𝙽𝙰s)
length(good[good == F]);

# mean total number of steps in a day
avgStepsPerday <- aggregate(steps ~ Day, DF2good, mean);
avgStepsMedian <- aggregate(steps ~ Day, DF2good, median);
totalStepsPerday <- aggregate(steps ~ Day, DF2good, sum);
plot(avgStepsPerday, type = 'l');
plot(avgStepsMedian, type = 'l');
plot(totalStepsPerday, type = 'l');

# histogram of the total number of steps taken each day
hist(totalStepsPerday[, 2]);

# analysis of daily pattern 
# Make a time series plot of the 5-minute interval (x- axis) 
# and the average number of steps taken, averaged across all days (y-axis)
avgStepsPerInterval <- aggregate(steps ~ interval, DF2good, mean);

# the time associated with the maximum average steps
avgStepsPerInterval[which.max(avgStepsPerInterval[, 2]),1];

avgStepsPerInterval[, 1] <- avgStepsPerInterval[, 1]/100;
plot(avgStepsPerInterval, type = 'l')

# filling the missing value 
# 1 - using mean value for each interval to fill them all
dataMissing <- DF2;
dataMissing[,1]<- tapply(dataMissing[,1], dataMissing[,3], function(x){
    x[is.na(x)] <- mean(x[!is.na(x)]);
})

avgStepsPerIntervalMissed <- aggregate(steps ~ interval, dataMissing, mean);

avgCompare <- data.frame(avgStepsPerInterval, missedAvg = avgStepsPerIntervalMissed[,2]);
plot(avgCompare[,1], avgCompare[,2], type = 'l', col = 'red')
lines(avgCompare[,1], avgCompare[,3], col = 'blue')


# pattern for weekdays and weekends
DFweekdays <- data.frame(dataMissing, wds <- weekdays(dataMissing[,2]));
names(DFweekdays)[5] <- c("wds");
weekdays1 <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday');
DFweekdays$wdays <- factor((DFweekdays$wds %in% weekdays1), 
                   levels=c(T, F), labels=c('weekday', 'weekend')); 

# average on interval by weekdays and weekends 
DFweekday <- subset(DFweekdays, wdays == "weekday");
avgStepsPerIntervalweekday <- 
    aggregate(steps ~ interval, DFweekday, mean);

DFweekend <- subset(DFweekdays, wdays == "weekend");
avgStepsPerIntervalweekend <- 
    aggregate(steps ~ interval, DFweekend, mean);

# panel plot to compare the performance from weekdays and weekends
par(mfrow = c(2,1), mar = c(4,4,2,1))
plot(avgStepsPerIntervalweekday)
plot(avgStepsPerIntervalweekend)


