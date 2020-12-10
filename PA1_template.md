\#\#Instructions

1.  Code for reading in the dataset and/or processing the data
2.  Histogram of the total number of steps taken each day
3.  Mean and median number of steps taken each day
4.  Time series plot of the average number of steps taken
5.  The 5-minute interval that, on average, contains the maximum number
    of steps
6.  Code to describe and show a strategy for imputing missing data
7.  Histogram of the total number of steps taken each day after missing
    values are imputed
8.  Panel plot comparing the average number of steps taken per 5-minute
    interval across weekdays and weekends
9.  All of the R code needed to reproduce the results (numbers, plots,
    etc.) in the report

<!-- -->

    knitr::opts_chunk$set(echo = TRUE)

    ##Preparing the Workspace
    library("lattice")
    library("data.table")
    library("ggplot2")
    library("dplyr")

    # Specify Working Directory
    #setwd("C:/Users/azizi/Desktop/Course Project 1")

    # Specify URL where file is stored
    #url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"

    # Specify destination where file should be saved
    #destfile <- "C:/Users/azizi/Desktop/Course Project 1/repdata%2Fdata%2Factivity.zip"

    # Apply download.file function in R
    #download.file(url, destfile)

    # Unzipping the file
    #unzip(zipfile="C:/Users/azizi/Desktop/Course Project 1/repdata%2Fdata%2Factivity.zip")

\#\#Loading and preprocessing the data

    activity <- read.csv("./activity.csv")
    summary(activity)

    ##      steps            date              interval     
    ##  Min.   :  0.00   Length:17568       Min.   :   0.0  
    ##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
    ##  Median :  0.00   Mode  :character   Median :1177.5  
    ##  Mean   : 37.38                      Mean   :1177.5  
    ##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
    ##  Max.   :806.00                      Max.   :2355.0  
    ##  NA's   :2304

\#\#1. What is mean total number of steps taken per day?

    totalSteps <- with(activity, aggregate(steps, by = list(date), sum, na.rm = TRUE))
    colnames(totalSteps) <- c("date", "steps")

    hist(totalSteps$steps, xlab="Total Number of Steps", ylab="No. of Days", main="Total Number of Steps Taken per Day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

\#Mean of the total number of steps taken per day

    mean(totalSteps$steps)

    ## [1] 9354.23

\#Median of the total number of steps taken per day

    median(totalSteps$steps)

    ## [1] 10395

\#\#2. What is the average daily activity pattern?

    pattern <- aggregate(activity$steps, by = list(activity$interval), mean, na.rm=TRUE)

    colnames(pattern) <- c("interval", "mean")

    plot(pattern$interval, pattern$mean, type = "l", xlab="5-Minute Interval", ylab="Average No. of Steps Taken", main="Average Daily Activity Pattern")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-6-1.png)

\#Which 5-minute interval, on average across all the days in the
dataset, contains the maximum number of steps?

    pattern[which.max(pattern[,2]),1]

    ## [1] 835

\#\#3. Imputing missing values

\#Total no. of missing values in the dataset

    sum(is.na(activity$steps))

    ## [1] 2304

\#Imputed by using mean of the 5-minute interval and rechecking total
no. of missing values after imputing

    imputed <- pattern$mean[match(activity$interval, pattern$interval)]

    sum(is.na(imputed))

    ## [1] 0

\#Histogram of the total number of steps taken each day after imputing

    imputed_5_min <- transform(activity, steps = ifelse(is.na(activity$steps), yes = imputed, no = activity$steps))
    totalStepsNew <- aggregate(steps ~ date, imputed_5_min, sum)
    colnames(totalStepsNew) <- c("date", "interval_steps")

    hist(totalStepsNew$interval_steps, xlab="Total Number of Steps", ylab="No. of Days", main="Total Number of Steps Taken per Day")

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-10-1.png)

\#Mean of the total number of steps taken per day after imputing

    mean(totalStepsNew$interval_steps)

    ## [1] 10766.19

\#Median of the total number of steps taken per day after imputing

    median(totalStepsNew$interval_steps)

    ## [1] 10766.19

\#The difference values between the original datasets and the imputed
datasets for mean & median

    difference_mean <- mean(totalStepsNew$interval_steps)  - mean(totalSteps$steps)
    difference_median <- median(totalStepsNew$interval_steps) - median(totalSteps$steps)

\#The difference values between the original datasets and the imputed
datasets for mean is;

    print(difference_mean)

    ## [1] 1411.959

\#and for median is;

    print(difference_median)

    ## [1] 371.1887

\#\#4. Are there differences in activity patterns between weekdays and
weekends?

\#Creating a new factor variable in the dataset with two levels –
“weekday” and “weekend”

    imputedNew<-imputed%>%
            mutate(dayType= ifelse(weekdays(imputed$date)=="Saturday" | weekdays(imputed$date)=="Sunday", "Weekend", "Weekday"))
    head(imputedNew)

    ##     steps       date interval dayType
    ## 1 37.3826 2012-10-01        0 Weekday
    ## 2 37.3826 2012-10-01        5 Weekday
    ## 3 37.3826 2012-10-01       10 Weekday
    ## 4 37.3826 2012-10-01       15 Weekday
    ## 5 37.3826 2012-10-01       20 Weekday
    ## 6 37.3826 2012-10-01       25 Weekday

\#Making a panel plot containing a time series plot

    averageAll<-imputedNew %>% group_by(dayType, interval) %>% summarize(averageByDay=sum(steps))

    ## `summarise()` regrouping output by 'dayType' (override with `.groups` argument)

    with(averageAll, xyplot(averageByDay ~ interval | dayType, type = "l"      
          , xlab = "Intervals"
          , ylab = "Average Number of Steps"))

![](PA1_template_files/figure-markdown_strict/unnamed-chunk-18-1.png)
