### Loading and Preprocessing the data

    library(dplyr)
    library(ggplot2)

    if(!file.exists("./data")){dir.create("./data")}
    fileUrl <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
    download.file(fileUrl, destfile = "./data/activitydata.zip", method = "curl")
    unzip(zipfile = "./data/activitydata.zip", exdir = "./data")
    activity <- read.csv("./data/activity.csv")

### What is the mean total number of steps taken per day?

1.  Making a histogram of the total number of steps taken each day

<!-- -->

    activity$date <- as.Date(activity$date)
    stepsperday <- group_by(activity, date) %>%
         summarize(total_steps = sum(steps, na.rm = TRUE))

    qplot(total_steps, data = stepsperday, na.rm = TRUE, 
          binwidth = 500, 
          xlab = "Total number of steps per day",
          ylab = "Frequency",
          main = "Histogram of the total number of steps taken each day")

![](PA1_template_files/figure-markdown_strict/steps%20per%20day-1.png)

1.  Calculate the **mean** and **median** total number of steps taken
    per day

<!-- -->

    meansteps <- stepsperday %>%
         summarize(average = mean(total_steps, na.rm = TRUE), median = median(total_steps, na.rm = TRUE))
    meansteps

    ## # A tibble: 1 x 2
    ##   average median
    ##     <dbl>  <int>
    ## 1   9354.  10395

### What is the average daily activity pattern?

1.  Make a time series plot of the 5-minute interval (x-axis) and the
    average number of steps taken, averaged across all days (y-axis)

<!-- -->

    dailyactivity <- activity %>% group_by(interval) %>% 
         summarize(average = mean(steps,na.rm = TRUE))

    qplot(interval, average,
          data = dailyactivity,geom="line",
          xlab = "5-minute intervals",
          ylab = "Average steps taken across all days")

![](PA1_template_files/figure-markdown_strict/daily%20activity%20pattern-1.png)

1.  Which 5-minute interval, on average across all the days in the
    dataset, contains the maximum number of steps?

<!-- -->

    dailyactivity[which.max(dailyactivity$average),]

    ## # A tibble: 1 x 2
    ##   interval average
    ##      <int>   <dbl>
    ## 1      835    206.

### Imputing missing values

1.  Calculate the total number of missing values

<!-- -->

    activityNA <- which(is.na(activity$steps))
    length(activityNA)

    ## [1] 2304

1.  Devise a strategy for filling in all of the missing values in the
    dataset

<!-- -->

    activity_noNA <- activity[which(!is.na(activity$steps)),]
    activity_avg <- activity_noNA %>%
         group_by(interval) %>%
         summarize(average = mean(steps))

    activity_avg$average <- as.integer(activity_avg$average)

    activityNA <- activity[which(is.na(activity$steps)),]

    activityNA$steps <- ifelse(activityNA$interval == activity_avg$interval, activity_avg$average)

1.  Create a new dataset with missing values filled in

<!-- -->

    activity_NA_filled <- rbind(activity_noNA, activityNA) %>% arrange(date)

1.  Make new histogram with imputed values and report **mean** and
    **median** total number of steps taken per day

<!-- -->

    stepsperday2 <- group_by(activity_NA_filled, date) %>%
         summarize(total_steps = sum(steps))

    qplot(total_steps, data = stepsperday2, 
          binwidth = 500, 
          xlab = "Total number of steps per day",
          ylab = "Frequency",
          main = "Histogram of the total number of steps taken each day (missing values imputed)")

![](PA1_template_files/figure-markdown_strict/non-NA%20histogram-1.png)

    meansteps2 <- stepsperday2 %>%
         summarize(average = mean(total_steps, na.rm = TRUE), median = median(total_steps, na.rm = TRUE))
    meansteps2

    ## # A tibble: 1 x 2
    ##   average median
    ##     <dbl>  <int>
    ## 1  10750.  10641

### Are there differences in activity patterns between weekdays and weekends?

1.  Create a new factor variable in the dataset with two levels --
    "weekday" and "weekend" indicating whether a given date is a weekday
    or weekend day.

<!-- -->

    activity$date <- as.Date(strptime(activity$date, format="%Y-%m-%d"))

    activity$datetype <- sapply(activity$date, function(x) {
         if (weekdays(x) == "Saturday" | weekdays(x) =="Sunday") 
         {y <- "Weekend"} else 
         {y <- "Weekday"}
         y
    })

1.  Make a time series plot of the 5-minute interval (x-axis) and the
    average number of steps taken, averaged across all weekday days or
    weekend days (y-axis)

<!-- -->

    activity_weekday <- aggregate(steps~interval + datetype, activity, mean, na.rm = TRUE)
    plot<- ggplot(activity_weekday, aes(x = interval , y = steps, color = datetype)) +
           geom_line() +
           labs(title = "Average daily steps by type of date", x = "Interval", y = "Average number of steps") +
           facet_wrap(~datetype, ncol = 1, nrow=2)
    print(plot)

![](PA1_template_files/figure-markdown_strict/weekday%20plot-1.png)
