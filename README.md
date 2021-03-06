---
title: "README - Reproducible Research: Peer Assessment 1"
output: 
html_document:
keep_md: true
echo : true

---
# Student: Al Shain
## Email: al_shain@me.com
#

# Steps required to reproduce this report:
## Loading and preprocessing the data
#### 1 set workding directory to project
#### 2 make sure the libraries for knitr and plotting are loaded
#### 3 set the download, and unzip file name
#### 4 test for data foloder and zip file, if NOT found create
#### 5 read in the csv data
### * What is mean total number of steps taken per day?
#### 1 Calculate the total number of steps taken per day
### * What is the average daily activity pattern?
#### 1 Calculate and report the mean and median of the total number of steps taken per day
#### 2 Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)
### * Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?
##  Imputing missing values
#### 1 Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs) 
#### 2 Devise a strategy for filling in all of the missing values in the dataset.
#### 3 Create a new dataset that is equal to the original dataset but with the missing data filled in.
#### 4 Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 
### * Do these values differ from the estimates from the first part of the assignment? 
### * What is the impact of imputing missing data on the estimates of the total daily number of steps?
### * Are there differences in activity patterns between weekdays and weekends?  
#### 1 Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.
#### 2 Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). 
