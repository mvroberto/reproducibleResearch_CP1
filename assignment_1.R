
library(dplyr)
library(ggplot2)
###########Read file

file <- read.csv("activity.csv")


#######################
#############  File Structure ############

# 'data.frame':	17568 obs. of  3 variables:
#   $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
# $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ interval: int  0 5 10 15 20 25 30 35 40 45 ...


#### names
# 1] "steps"    "date"     "interval"

#####NAS
 ## Steps : 2304
 ## date: 0
 ## interval: 0

############ Non below 0

# file$date <- as.character(file$date
#                         )
file_dates <-  group_by(file,date) %>% summarise(steps_per_day = sum(steps), mean_steps = mean(steps), median_steps = median(steps, na.rm = TRUE))
# file$date <- as.Date(file$date, format = "%Y %M %D")

a_sum_steps <- ggplot(file_dates,aes(date,steps_per_day))
b_mean_steps <- ggplot(file_dates,aes(date,mean_steps))
median_steps_total <- median(file_dates$steps_per_day, na.rm = TRUE)


# file_interval <- file
# file_interval$interval <- as.factor(file_interval$interval)
# file_interval_summirised <-  group_by(file,interval) %>% summarise(steps_per_day == sum(steps))

file_interval <-  group_by(file,interval) %>% summarise(steps_per_day = sum(steps, na.rm = TRUE), mean_steps = mean(steps,na.rm = TRUE), median_steps = median(steps, na.rm = TRUE))
c <- ggplot(data = file_interval, aes(interval,steps_per_day))
plot_series_interval <- c + geom_smooth()

ordered_interval_file <- file_interval[with(file_interval,order(-steps_per_day)),]