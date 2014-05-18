setwd("C:/Users/David/Dropbox/Coursera/Reproducible Research/Peer Assessment 1")
activity<- read.csv("./repdata_data_activity/activity.csv", colClasses=c("integer","Date","integer"))

total.steps.per.day<- tapply(activity$steps, activity$date, sum)
hist(total.steps.per.day, breaks=10, main="Histogram of no of steps taken each day")

mean.steps.per.day<- mean(total.steps.per.day,na.rm=TRUE)
median.steps.per.day<- median(total.steps.per.day,na.rm=TRUE)

mean.steps.per.interval<- tapply(activity$steps,activity$interval,mean,na.rm=TRUE)
plot(names(mean.steps.per.interval),mean.steps.per.interval,type="l",xlab="Time of Day (hhmm)",ylab="Mean no of steps")
max.interval<- names(which.max(mean.steps.per.interval))

num.missing<- sum(is.na(activity$steps))
mean.steps.per.interval<- tapply(activity$steps,activity$interval,mean,na.rm=TRUE)
activity.no.missing<- merge(activity,cbind(mean.steps.per.interval,row=row.names(mean.steps.per.interval)),by.x="interval",by.y="row",all.x=TRUE)
activity.no.missing$mean.steps.per.interval<- as.numeric(as.character(activity.no.missing$mean.steps.per.interval)) 
activity$steps[is.na(activity$steps)]<- activity.no.missing$mean.steps.per.interval[is.na(activity$steps)]

total.steps.per.day<- tapply(activity$steps, activity$date, sum)
hist(total.steps.per.day, breaks=10, main="Histogram of no of steps taken each day")

mean.steps.per.day<- mean(total.steps.per.day,na.rm=TRUE)
median.steps.per.day<- median(total.steps.per.day,na.rm=TRUE)

activity$wday<-weekdays(activity$date)
activity$day.type<- factor(ifelse((activity$wday =="Saturday" | activity$wday =="Sunday"),"weekend","weekday"))

activity<- activity[order(activity$date,activity$interval),]
mean.steps.per.interval.per.day.type<- tapply(activity$steps,list(activity$day.type,activity$interval),mean,na.rm=TRUE)
mean.steps.per.interval.per.day.type2<- t(mean.steps.per.interval.per.day.type)
mean.steps.per.interval.per.day.type3<- as.data.frame(mean.steps.per.interval.per.day.type2)
mean.steps.per.interval.per.day.type3$interval<- row.names(mean.steps.per.interval.per.day.type3)

par(mfcol=c(2,1))
plot(mean.steps.per.interval.per.day.type3$interval,mean.steps.per.interval.per.day.type3$weekday,type="l",xlab="Time of Day (hhmm)",ylab="No of steps",main="Weekdays")
plot(mean.steps.per.interval.per.day.type3$interval,mean.steps.per.interval.per.day.type3$weekend,type="l",xlab="Time of Day (hhmm)",ylab="No of steps",main="Weekends")


