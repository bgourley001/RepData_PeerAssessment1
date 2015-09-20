#RepData_Assignment1

#local working directory : I:/Coursera/ReproducibleResearch/Assignment1/RepData_PeerAssessment1

#load required libraries
library(dplyr)
library(ggplot2)

############################################################################################
# Loading and preprocessing the raw data
############################################################################################
#read in the raw data from the activity folder
activity.raw <- read.csv("activity/activity.csv")

############################################################################################
# Calculate Mean and median Values of the Total steps per day
############################################################################################
#convert date strings to R dates
activity.raw$date <- as.Date(activity.raw$date,"%Y-%m-%d")

#group activity.raw by date
activity.raw.byDate <- group_by(activity.raw,date)

#Calculate Total Steps per day, plot as a histogram and report the mean and median
sumSteps.raw <- summarize(activity.raw.byDate,
          total.steps.perDay = sum(steps,na.rm = TRUE),
          average.steps.perDay = mean(total.steps.perDay,na.rm = TRUE))

#Summary
s <- summary(sumSteps.raw$total.steps.perDay)
print(s)

#histogram
hist(sumSteps.raw$total.steps.perDay,main = "Total Steps per Day",xlab = "Total Steps")
abline(v = s[4],col="red",lwd=2)
abline(v = s[3],col="blue",lwd=2)
legend("topright",legend = c("Mean","Median"),lty=c(1,1),lwd=c(2,2),col=c("red","blue"))

#Mean and Median Values
print(paste("Mean of Total Steps Taken per Day = ",s[4],sep = ""))

print(paste("Median of Total Steps Taken per Day = ",s[3],sep = ""))

############################################################################################
#time-series plot of average steps per time interval
############################################################################################
#group by interval
activity.raw.byInterval <- group_by(activity.raw,interval)
intervalSteps <- summarize(activity.raw.byInterval,
                      avg.steps.perInterval = mean(steps,na.rm = TRUE))

#Highest no of steps
max.steps <- max(intervalSteps$avg.steps.perInterval)

#Interval containing the highest no of steps
max.interval.steps <- intervalSteps$interval[which.max(intervalSteps$avg.steps.perInterval)]

#Daily Activity Pattern Plot
plot(intervalSteps$interval,intervalSteps$avg.steps.perInterval,type = "l", lty = 1,
     lwd = 1.5,col="blue",
     main="Daily Activity Pattern",xlab = "Interval",ylab = "Average Steps")

#add vertical line indicating the interval with the highest number of steps
abline(v=max.interval.steps,lty=2,lwd=1.5,col="red")
legend("topright",legend = "Highest Steps",lty=2,lwd=1.5,col="red")

print(paste("Interval which contains the highest number of steps (",
            round(max.steps,0),") is interval : ",
            max.interval.steps,sep = "")) 

##############################################################################################
#Impute Missing Values
##############################################################################################
#Replace missing step values with mean for the corresponding 5 minute interval
#copy the raw dataset
activity.clean <- activity.raw

#extract the NA's
steps.na <- subset(activity.raw,is.na(steps))

#Get number of missing days
missing.days <- length(unique(steps.na$date))

#replace NAs with those in intervalSteps replicating by no of missing days
steps.na$steps <- rep(intervalSteps$avg.steps.perInterval,missing.days)

#replace the NA's in the activity dataset
activity.clean$steps <- replace(activity.clean$steps,is.na(activity.clean$steps),steps.na$steps)

summary(activity.clean)

#group activity.clean by date
activity.clean.byDate <- group_by(activity.clean,date)

#Calculate Total Steps per day, plot as a histogram and report the mean and median
sumSteps.clean <- summarize(activity.clean.byDate,
                            total.steps.perDay = sum(steps,na.rm = TRUE),
                            average.steps.perDay = mean(total.steps.perDay,na.rm = TRUE))

#print summary
s <- summary(sumSteps.clean$total.steps.perDay)
print(s)

#histogram
hist(sumSteps.clean$total.steps.perDay,main = "Total Steps per Day",xlab = "Total Steps")
abline(v = s[4],col="red",lwd=2)
abline(v = s[3],col="blue",lwd=2)
legend("topright",legend = c("Mean","Median"),lty=c(1,1),lwd=c(2,2),col=c("red","blue"))

print(paste("Mean of Total Steps Taken per Day = ",s[4],sep = ""))

print(paste("Median of Total Steps Taken per Day = ",s[3],sep = ""))

#############################################################################################
# Weeday/Weekend activity Comparison
#############################################################################################
#add a column indicating day of the week and a column indicating weekday or weekend
activity.clean <- mutate(activity.clean,
                         day = weekdays(activity.clean$date),
                         period = ifelse(day == "Saturday" | day == "Sunday",
                                         "weekend","weekday"))

#convert period to a factor variable
activity.clean$period <- as.factor(activity.clean$period)

#time-series panel plot of average steps per weekend/weekday time interval
#group by period and interval
activity.clean.byPeriod <- group_by(activity.clean,period,interval)
intervalSteps <- summarize(activity.clean.byPeriod,
                           avg.steps.perInterval = mean(steps,na.rm = TRUE))

ggplot(intervalSteps,aes(interval,avg.steps.perInterval)) +
    geom_line(group = 1) + 
    facet_wrap(~ period,ncol = 1) +
    ggtitle("Weekday/Weekend Activity Comparison") +
    labs(x="Interval",y="Average Steps Per Interval")


