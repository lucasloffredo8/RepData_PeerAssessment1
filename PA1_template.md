---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

x <- read.csv("activity.csv")

## What is mean total number of steps taken per day?

dates1 <- levels(x[["date"]])
y <- transform(x, date = as.character(date))
stepSumList <- as.character(vector())
for(i in dates1){
  eachDay <- y[y$date == i, ]
  daySum <- sum(eachDay[["steps"]])
  stepSumList <- c(stepSumList, daySum)
}
Steps <- as.numeric(stepSumList)
hist(Steps)
median(Steps, na.rm = TRUE)
mean(Steps, na.rm = TRUE)

## What is the average daily activity pattern?

q <- x[1:288, ]
ints1 <- q[["interval"]]
Steps1 <- as.numeric(vector())
for(i in ints1){
  eachInt <- x[x$interval == i, ]
  intAvg <- mean(eachInt[["steps"]], na.rm = TRUE)
  Steps1 <- c(Steps1, intAvg)
}
plot(ints1, Steps1)
gg <- cbind(as.data.frame(ints1), as.data.frame(Steps1))
hh <- gg[gg$Steps1 == max(gg[["Steps1"]]), ]
hh[["ints1"]]

## Imputing missing values

test <- is.na(x[["steps"]])
length(test[test == TRUE])
q <- x[1:288, ]
ints1 <- q[["interval"]]
xAll <- read.csv("activity.csv")
for(i in ints1){
  eachInt <- x[x$interval == i, ]
  intAvg <- mean(eachInt[["steps"]], na.rm = TRUE)
  xAll[is.na(xAll) & xAll$interval == i] <- intAvg
}
dates1All <- levels(xAll[["date"]])
yAll <- transform(xAll, date = as.character(date))
stepSumListAll <- as.character(vector())
for(i in dates1All){
  eachDayAll <- yAll[yAll$date == i, ]
  daySumAll <- sum(eachDayAll[["steps"]])
  stepSumListAll <- c(stepSumListAll, daySumAll)
}
StepsAll <- as.numeric(stepSumListAll)
hist(StepsAll)
median(StepsAll)
mean(StepsAll)

## Are there differences in activity patterns between weekdays and weekends?

xx <- transform(xAll, date = strptime(x[, "date"], "%Y-%m-%d"))
xxx <- cbind(xAll, weekdayweekend = weekdays(xx[["date"]]))
xxxx <- transform(xxx, weekdayweekend = as.character(weekdayweekend))
xxxx$weekdayweekend[xxxx$weekdayweekend %in% "Monday"] <- "Weekday"
xxxx$weekdayweekend[xxxx$weekdayweekend %in% "Tuesday"] <- "Weekday"
xxxx$weekdayweekend[xxxx$weekdayweekend %in% "Wednesday"] <- "Weekday"
xxxx$weekdayweekend[xxxx$weekdayweekend %in% "Thursday"] <- "Weekday"
xxxx$weekdayweekend[xxxx$weekdayweekend %in% "Friday"] <- "Weekday"
xxxx$weekdayweekend[xxxx$weekdayweekend %in% "Saturday"] <- "Weekend"
xxxx$weekdayweekend[xxxx$weekdayweekend %in% "Sunday"] <- "Weekend"
xxxxx <- transform(xxxx, weekdayweekend = as.factor(weekdayweekend))
xWeekday <- xxxxx[xxxxx$weekdayweekend == "Weekday", ]
xWeekend <- xxxxx[xxxxx$weekdayweekend == "Weekend", ]
q <- x[1:288, ]
ints <- q[["interval"]]
stepsWeekday <- as.numeric(vector())
for(i in ints){
  eachIntWd <- xWeekday[xWeekday$interval == i, ]
  intAvgWd <- mean(eachIntWd[["steps"]])
  stepsWeekday <- c(stepsWeekday, intAvgWd)
}
plot(ints, stepsWeekday)
stepsWeekend <- as.numeric(vector())
for(i in ints){
  eachIntWe <- xWeekend[xWeekend$interval == i, ]
  intAvgWe <- mean(eachIntWe[["steps"]])
  stepsWeekend <- c(stepsWeekend, intAvgWe)
}
plot(ints, stepsWeekend)
