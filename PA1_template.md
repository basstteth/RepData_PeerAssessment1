Untitled
================

Loading and preprocessing the data
----------------------------------

activityData &lt;- read.csv(file="activity.csv", header=TRUE)

What is mean total number of steps taken per day?
-------------------------------------------------

totalSteps &lt;- aggregate(steps ~ date, activityData, FUN=sum)

hist(totalSteps$steps, main = "Total Steps per Day", xlab = "Number of Steps")

meanSteps &lt;- mean(totalSteps*s**t**e**p**s*, *n**a*.*r**m* = *T**R**U**E*)*m**e**d**S**t**e**p**s* &lt; −*m**e**d**i**a**n*(*t**o**t**a**l**S**t**e**p**s*steps, na.rm = TRUE)

What is the average daily activity pattern?
-------------------------------------------

library(ggplot2) meanStepsByInt &lt;- aggregate(steps ~ interval, activityData, mean) ggplot(data = meanStepsByInt, aes(x = interval, y = steps)) + geom\_line() + ggtitle("Average Daily Activity Pattern") + xlab("5-minute Interval") + ylab("Average Number of Steps") + theme(plot.title = element\_text(hjust = 0.5))

maxInt &lt;- meanStepsByInt\[which.max(meanStepsByInt$steps),\]

Imputing missing values
-----------------------

imp\_activityData &lt;- transform(activityData, steps = ifelse(is.na(activityData*s**t**e**p**s*),*m**e**a**n**S**t**e**p**s**B**y**I**n**t*steps\[match(activityData*i**n**t**e**r**v**a**l*, *m**e**a**n**S**t**e**p**s**B**y**I**n**t*interval)\], activityData$steps))

impStepsByInt &lt;- aggregate(steps ~ date, imp\_activityData, FUN=sum) hist(impStepsByInt$steps, main = "Imputed Number of Steps Per Day", xlab = "Number of Steps")

impMeanSteps &lt;- mean(impStepsByInt*s**t**e**p**s*, *n**a*.*r**m* = *T**R**U**E*)*i**m**p**M**e**d**S**t**e**p**s* &lt; −*m**e**d**i**a**n*(*i**m**p**S**t**e**p**s**B**y**I**n**t*steps, na.rm = TRUE) diffMean = impMeanSteps - meanSteps diffMed = impMedSteps - medSteps diffTotal = sum(impStepsByInt*s**t**e**p**s*)−*s**u**m*(*t**o**t**a**l**S**t**e**p**s*steps)

Are there differences in activity patterns between weekdays and weekends?
-------------------------------------------------------------------------

DayType &lt;- function(date) { day &lt;- weekdays(date) if (day %in% c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')) return ("weekeday") else if (day %in% c('Saturday', 'Sunday')) return ("weekend") else stop ("Invalid Date Format.") } imp\_activityData*d**a**t**e* &lt; −*a**s*.*D**a**t**e*(*i**m**p*<sub>*a*</sub>*c**t**i**v**i**t**y**D**a**t**a*date) imp\_activityData*d**a**y* &lt; −*s**a**p**p**l**y*(*i**m**p*<sub>*a*</sub>*c**t**i**v**i**t**y**D**a**t**a*date, FUN = DayType)

meanStepsByDay &lt;- aggregate(steps ~ interval + day, imp\_activityData, mean) ggplot(data = meanStepsByDay, aes(x = interval, y = steps)) + geom\_line() + facet\_grid(day ~ .) + ggtitle("Average Daily Activity Pattern") + xlab("5-minute Interval") + ylab("Average Number of Steps") + theme(plot.title = element\_text(hjust = 0.5))
