##
## Student: Al Shain
## Email: al_shain@me.com
## Course Project Assignment: Reproducible Research Peer Assessment 1
## Description: This assignment will be described in multiple parts. You will need to write a report that answers the questions detailed below. Ultimately, you will need to complete the entire assignment in a single R markdown document that can be processed by knitr and be transformed into an HTML file.

## Throughout your report make sure you always include the code that you used to
## generate the output you present. When writing code chunks in the R markdown
## document, always use echo = TRUE so that someone else will be able to read
## the code. This assignment will be evaluated via peer assessment so it is
## essential that your peer evaluators be able to review the code for your
## analysis.
## For the plotting aspects of this assignment, feel free to use any plotting
## system in R (i.e., base, lattice, ggplot2)
## Fork/clone the GitHub repository created for this assignment. You will submit
## this assignment by pushing your completed files into your forked repository
## on GitHub. The assignment submission will consist of the URL to your GitHub
## repository and the SHA-1 commit ID for your repository state.


## set workding directory to project
setwd("~/Desktop/Coursera/ReproducibleResearch/My-Reproducible-Research-Peer-Assessment-1")

## make sure the libraries for knitr and plotting are loaded
library(knitr)
library(ggplot2)

## set the download, and unzip file name
downloadFile <- "data/activity.zip"
downloadURL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

## test for data foloder and zip file, if NOT found create
if(!file.exists("./data")) { dir.create("./data")}
if (!file.exists(downloadFile)) {
  download.file(downloadURL, downloadFile, method = "curl");
  unzip(downloadFile, overwrite = T, exdir = ".")
}

## read in the csv data
data <- read.csv("./data/activity.csv")

## Calculate the total number of steps taken per day
total.steps <- tapply(data$steps, data$date, FUN=sum, na.rm=TRUE)

## Make a histogram of the total number of steps taken each day
qplot(total.steps, binwidth=1000, colour=I("blue"), fill=I("green"), xlab="Total Number of Steps Taken Each Day")

## Calculate and report the mean and median of the total number of steps taken per day
mean(total.steps, na.rm=TRUE)  
median(total.steps, na.rm=TRUE)  

## Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis)
## and the average number of steps taken, averaged across all days (y-axis)
averageSteps <- aggregate(x=list(steps=data$steps), by=list(interval=data$interval),
                      FUN=mean, na.rm=TRUE)
ggplot(data=averageSteps, aes(x=interval, y=steps)) +
  geom_line(colour="blue", size=1.5) +
  xlab("5 Minute Interval") +
  ylab("Average Number of Steps Taken")

## Which 5-minute interval, on average across all the days in the dataset,
## contains the maximum number of steps?
averageSteps[which.max(averageSteps$steps),]

## Calculate and report the total number of missing values in the dataset (i.e.
## the total number of rows with NAs) 
missingValues <- is.na(data$steps)
table(missingValues)


## Devise a strategy for filling in all of the missing values in the dataset.
## The strategy does not need to be sophisticated. For example, you could use
## the mean/median for that day, or the mean for that 5-minute interval, etc.
fill.value <- function(steps, interval) {
  filled <- NA
  if (!is.na(steps))
    filled <- c(steps)
  else
    filled <- (averageSteps[averageSteps$interval==interval, "steps"])
  return(filled)
}

## Create a new dataset that is equal to the original dataset but with the
## missing data filled in.
filled.data <- data
filled.data$steps <- mapply(fill.value, filled.data$steps, filled.data$interval)

## Make a histogram of the total number of steps taken each day and Calculate
## and report the mean and median total number of steps taken per day. 
total.steps <- tapply(filled.data$steps, filled.data$date, FUN=sum)
qplot(total.steps, binwidth=500, xlab="Total Number of Steps Taken Each Day", colour=I("blue"), fill=I("green"))

## Do these values differ from the estimates from the first part of the
## assignment? What is the impact of imputing missing data on the estimates of
## the total daily number of steps?
mean(total.steps)
median(total.steps)

## Create a new factor variable in the dataset with two levels – “weekday” and
## “weekend” indicating whether a given date is a weekday or weekend day.
weekday.or.weekend <- function(date) {
  day <- weekdays(date)
  if (day %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday"))
    return("weekday")
  else if (day %in% c("Saturday", "Sunday"))
    return("weekend")
  else
    stop("invalid date")
}
filled.data$date <- as.Date(filled.data$date)
filled.data$day <- sapply(filled.data$date, FUN=weekday.or.weekend)

## Make a panel plot containing a time series plot (i.e. type = "l") of the
## 5-minute interval (x-axis) and the average number of steps taken, averaged
## across all weekday days or weekend days (y-axis). 
averages <- aggregate(steps ~ interval + day, data=filled.data, mean)
ggplot(averages, aes(interval, steps)) + geom_line() + facet_grid(day ~ .) +
  xlab("5 Minute Interval") + ylab("Number of Steps") +
  geom_line(colour="blue", size=1.25)



