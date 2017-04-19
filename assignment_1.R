
library(dplyr)
library(ggplot2)
library(knitr)
###########Read file

file <- read.csv("activity.csv")

file_with_no_na <- file
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
mean_steps <- mean(file_dates$steps_per_day, na.rm = TRUE)
median_steps_total <- median(file_dates$steps_per_day, na.rm = TRUE)


# file_interval <- file
# file_interval$interval <- as.factor(file_interval$interval)
# file_interval_summirised <-  group_by(file,interval) %>% summarise(steps_per_day == sum(steps))

file_interval <-  group_by(file,interval) %>% summarise(steps_per_day = sum(steps, na.rm = TRUE), mean_steps = mean(steps,na.rm = TRUE), median_steps = median(steps, na.rm = TRUE))
c <- ggplot(data = file_interval, aes(interval,steps_per_day))
plot_series_interval <- c + geom_smooth()

ordered_interval_file <- file_interval[with(file_interval,order(-steps_per_day)),]


############### Second part

na_values <- table(is.na(file$steps))
percentage_of_nas <- (na_values[[2]]/ length(file$steps))*100


#### missing value data base
file_interval <- mutate(file_interval, steps_rounded = round(mean_steps))

i <- 1
list <- list()
a <- c(1: na_values[[2]])
steps <- file$steps
interval <-  file$interval
steps_means <- file_interval$steps_rounded
interval_means <- file_interval$interval



for( x in steps){
  if(is.na(steps[i])){
    
    file_with_no_na$steps[i] <- steps_means[which(interval_means  %in% interval[i])]
    
  }
  
  i <- i + 1
  
}

# 
z <- ggplot(file_with_no_na,aes(steps)) + geom_histogram(binwidth = 55)
total_step_mean <- mean(file_with_no_na$steps)
total_step_median <- median(file_with_no_na$steps)
file_interval_summed <-  group_by(file_with_no_na,date) %>% summarise(steps_per_day = sum(steps), mean_steps = mean(steps), median_steps = median(steps, na.rm = TRUE))


####
## weekdays

file_with_no_na$day  <- weekdays(as.Date(file_with_no_na$date))
file_with_no_na_weekday <- mutate(file_with_no_na, weekday = ifelse(day %in% c("Saturday","Sunday"),"Weekend", "Weekday"))

w <- ggplot(file_with_no_na_weekday, aes(interval,steps)) + geom_line()

plot_weekdays <- w  + facet_grid(weekday ~ .)

file_dates$date <- as.Date(file_dates$date)
x <- ggplot(file_dates, aes(steps_per_day))
plot_hist <- x + geom_histogram()