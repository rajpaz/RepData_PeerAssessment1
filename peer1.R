library(ggplot2)
library(dplyr)
library(lattice)

df1 = read.csv("activity.csv")
df1[,2]=as.factor(df1[,2])
df1[,3]=as.factor(df1[,3])

dailyStepTable = df1[,1:2] %>% group_by(date) %>%summarize(dailySteps = sum(steps,na.rm=TRUE))
hist(dailyStepTable$dailySteps,main = paste("Histogram of Daily Steps"))
meanDailySteps = mean(dailyStepTable$dailySteps)
medianDailySteps = median(dailyStepTable$dailySteps)
sprintf("Mean Daily Steps: %f \n Median Daily Steps: %f",meanDailySteps,medianDailySteps)

# interval step table
intervalStepTable = df1[,c(1,3)] %>% group_by(interval) %>%summarize(intervalSteps = mean(steps,na.rm=TRUE))
xyplot(intervalStepTable$intervalSteps~intervalStepTable$interval, xlab="interval-index",ylab="Average-steps",type="l")


## Find interval where the maximum is achieved
which(intervalStepTable$intervalSteps==max(intervalStepTable$intervalSteps))

# Find the number of rows with NA
sum(is.na(df1))

# use mean interval values to replace NA
# collect all elements that have NAs
naCols = which(is.na(df1$steps))

# replace them with meadian values for those intervals

df1$steps[naCols]=sapply(naCols,function(x) intervalStepTable$intervalSteps[which(intervalStepTable$interval==df1$interval[x])] )
dailyStepTableImpute = df1 %>% group_by(date) %>%summarize(dailySteps = sum(steps,na.rm=TRUE))
hist(dailyStepTableImpute$dailySteps,main = paste("Histogram of Daily Steps"))
meanDailyStepsImpute = mean(dailyStepTableImpute$dailySteps)
medianDailyStepsImpute = median(dailyStepTableImpute$dailySteps)
c(meanDailySteps,meanDailyStepsImpute)
c(medianDailySteps,medianDailyStepsImpute)
cat(sprintf("Mean without imputing: %.2f Mean with imputing: %.2f: \nMedian without imputing: %.2f Median with imputing: %.2f",
            meanDailySteps,meanDailyStepsImpute,medianDailySteps,medianDailyStepsImpute))
```

# testing some theories whether substiuting with average steps per days matters
activity_df = read.csv("activity.csv")
activity_df[,2]=as.factor(activity_df[,2])
activity_df[,3]=as.factor(activity_df[,3])

dates_df = activity_df[,1:2] %>% group_by(date) %>% summarize(sum(steps,na.rm=TRUE))
# days where there are 0 steps
zeroDays=dates_df$date[which(dates_df$dailySteps==0)]
# days with NAs
naDays = unique(activity_df$date[which(is.na(activity_df$steps))])
# are they the same
naDays==zeroDays
# Yes, they are they same. If I impute based on using daily average then I am replacing zeros with zeros.


# create factor based on weekday and weekend
df1[,"weeknd"]= sapply(weekdays(as.Date(df1$date)), function(x) if (x == "Sunday" || x == "Saturday") "weekend" else "weekday")
df1[,"weeknd"]=as.factor(df1[,"weeknd"])

summaryStats = df1 %>% group_by(weeknd,interval) %>%summarize(dailySteps = mean(steps,na.rm=TRUE))
scales=list(y=list(tick.number=10),x=list(tick.number=10))
xyplot(summaryStats$dailySteps~summaryStats$interval|summaryStats$weeknd,layout=c(1,2),
       scales=list(y=list(seq(0,250,by=10)),x=list(seq(1,250,by=10))),
       xlab="interval-index",ylab="Average-steps",type="l")