
>  setwd("C:/Users/Karen/PA assignment")
> if(!file.exists("activity.csv")) {
+     getzip <- tempfile()
+     download.file("http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip",getzip)
+     unzip(getzip)
+     unlink(getzip)
+ }
> data <- read.csv("activity.csv", header = TRUE, sep = ',')

> library (lubridate)

Attaching package: ‘lubridate’

The following object is masked from ‘package:base’:

    date

Warning message:
package ‘lubridate’ was built under R version 3.3.3 
> data$date <- ymd(data$date)
> dim (data)
[1] 17568     3
> summary(data)
     steps             date               interval     
 Min.   :  0.00   Min.   :2012-10-01   Min.   :   0.0  
 1st Qu.:  0.00   1st Qu.:2012-10-16   1st Qu.: 588.8  
 Median :  0.00   Median :2012-10-31   Median :1177.5  
 Mean   : 37.38   Mean   :2012-10-31   Mean   :1177.5  
 3rd Qu.: 12.00   3rd Qu.:2012-11-15   3rd Qu.:1766.2  
 Max.   :806.00   Max.   :2012-11-30   Max.   :2355.0  
 NA's   :2304                                          
> steps_per_day <- aggregate(steps ~ date, data, sum, na.rm=TRUE)
> hist(steps_per_day$steps, main = "Total Steps Each Day", col="red", xlab="Number of Steps")
> steps_per_day_Mean <- mean(steps_per_day$steps)
> steps_per_day_Mean 
[1] 10766.19
> steps_per_day_Median <- median(steps_per_day$steps)
> steps_per_day_Median 
[1] 10765
> 
> 
> steps_per_interval <- aggregate(steps ~ interval, data, mean, na.rm=TRUE)
> plot(steps_per_interval$interval, steps_per_interval$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Day by Interval")
> max_interval <- steps_per_interval[which.max(steps_per_interval$steps),1]
> max_interval 
[1] 835

> sum(is.na(data$steps))
[1] 2304

> steps_per_interval_Mean<-function(interval){
+     steps_per_interval[steps_per_interval$interval==interval,]$steps
+ }
> Imputed_data <- data
> for(i in 1:nrow(Imputed_data)){
+     if(is.na(Imputed_data[i,]$steps)){
+         Imputed_data[i,]$steps <- steps_per_interval_Mean(Imputed_data[i,]$interval)
+     }
+ }
> sum(is.na(Imputed_data$steps))
[1] 0


> total_Steps_Per_Day_Imputed <- aggregate(steps ~ date, data=Imputed_data, sum)
> hist(total_Steps_Per_Day_Imputed$steps, main = "Total Steps Each Day (NAs replaced by mean value)", col="red", xlab="Number of Steps")
>
> mean_Steps_Per_Day_Imputed <- mean(total_Steps_Per_Day_Imputed$steps)
> mean_Steps_Per_Day_Imputed
[1] 10766.19
> median_Steps_Per_Day_Imputed <- median(totalStepsPerDayImputed$steps)
> median_Steps_Per_Day_Imputed
[1] 10766.19

>data_weekdays <- cbind(data, 
                      daytype=ifelse(weekdays(data$date) == "Saturday" | 
                                         weekdays(data$date) == "Sunday", "weekend", 
                                     "weekday"))

>steps_daytype <- aggregate(steps ~ interval + daytype, data_weekdays, mean)

> xyplot(steps ~ interval | daytype, steps_by_interval_i, type = "l", layout = c(1, 2), 
       xlab = "Interval", ylab = "Number of steps")
