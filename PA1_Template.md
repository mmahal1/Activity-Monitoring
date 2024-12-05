## Loading and Preprocessing the Data

    # Load the data
    data <- read.csv("activity.csv")
    # Convert the data column to Date Type
    data$date <- as.Date(data$date, format="%Y-%m-%d")

## Mean Total Number of Steps Taken Per Day

    # Calculate the total number of steps taken per day
    total_steps_per_day <- aggregate(steps ~ date, data, sum, na.rm = TRUE)

    # Make a histogram of the total number of steps taken each day
    hist(total_steps_per_day$steps, main="Total Steps per Day", xlab="Steps", breaks=20)

![](PA1_Template_files/figure-markdown_strict/unnamed-chunk-2-1.png)

    # Calculate and report the mean and median of the total number of steps taken per day
    mean_steps <- mean(total_steps_per_day$steps)
    median_steps <- median(total_steps_per_day$steps)

    mean_steps

    ## [1] 10766.19

    median_steps

    ## [1] 10765

## Average Daily Activity Pattern

    # Calculate the average number of steps taken in each 5-minute interval across all days
    avg_steps_per_interval <- aggregate(steps ~ interval, data, mean, na.rm = TRUE)

    # Make a time series plot of the 5-minute interval and the average number of steps taken
    plot(avg_steps_per_interval$interval, avg_steps_per_interval$steps, type="l", 
         main="Average Daily Activity Pattern", xlab="5-minute Interval", ylab="Average Number of Steps")

![](PA1_Template_files/figure-markdown_strict/unnamed-chunk-3-1.png)

    # Find the 5-minute interval that, on average, contains the maximum number of steps
    max_interval <- avg_steps_per_interval[which.max(avg_steps_per_interval$steps), ]
    max_interval

    ##     interval    steps
    ## 104      835 206.1698

## Imputing Missing Values

    # Calculate the total number of missing values in the dataset
    total_missing <- sum(is.na(data$steps))
    total_missing

    ## [1] 2304

    # Devise a strategy for filling in all of the missing values in the dataset
    fill_missing <- function(steps, interval) {
      if (is.na(steps)) {
        return(avg_steps_per_interval[avg_steps_per_interval$interval == interval, "steps"])
      } else {
        return(steps)
      }
    }

    # Create a new dataset that is equal to the original dataset but with the missing data filled in
    data_filled <- data
    data_filled$steps <- mapply(fill_missing, data$steps, data$interval)

    # Make a histogram of the total number of steps taken each day after missing values are imputed
    total_steps_per_day_filled <- aggregate(steps ~ date, data_filled, sum)
    hist(total_steps_per_day_filled$steps, main="Total Steps per Day (Imputed)", xlab="Steps", breaks=20)

![](PA1_Template_files/figure-markdown_strict/unnamed-chunk-4-1.png)

    # Calculate and report the mean and median total number of steps taken per day after missing values are imputed
    mean_steps_filled <- mean(total_steps_per_day_filled$steps)
    median_steps_filled <- median(total_steps_per_day_filled$steps)

    mean_steps_filled

    ## [1] 10766.19

    median_steps_filled

    ## [1] 10766.19

## Differences in Activity Patterns Between Weekdays and Weekends

    # Create a new factor variable in the dataset with two levels – “weekday” and “weekend”
    data_filled$day_type <- ifelse(weekdays(data_filled$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")

    # Make a panel plot containing a time series plot of the 5-minute interval and the average number of steps taken
    library(ggplot2)

    ## Warning: package 'ggplot2' was built under R version 4.4.2

    avg_steps_per_interval_day_type <- aggregate(steps ~ interval + day_type, data_filled, mean)

    ggplot(avg_steps_per_interval_day_type, aes(x=interval, y=steps, color=day_type)) +
      geom_line() +
      facet_wrap(~day_type, ncol=1) +
      labs(title="Average Steps per 5-minute Interval by Day Type", x="5-minute Interval", y="Average Number of Steps")

![](PA1_Template_files/figure-markdown_strict/unnamed-chunk-5-1.png)
