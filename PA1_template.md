### Loading and formatting the data

To load the document, first set the working directory to where the csv file is saved, then call the read.csv function

```{r, echo=TRUE}
#Need to set the working directory to where the "activity.csv" file is saved
library("ggplot2")
library("gridExtra")
setwd("C:/Users/daniel.williams/Documents/R/Class 5")
dataset=read.csv("activity.csv")
dataset[,"date"]=as.Date(dataset[,"date"],"%Y-%m-%d")
```

### Q: What is the mean total number of steps taken per day? 

To determine the total number of steps per day we can call the aggregate function by date.

```{r, echo=TRUE}
## Mean Steps Per Day
mean.day=aggregate(steps~date,data=dataset,FUN=sum)
mean(mean.day[,2])
## Median Steps Per Day
median(mean.day[,2])
## Histogram of Steps Per Day
qplot(mean.day$steps,geom="histogram",bins=25,main="Histogram of Steps/Day",xlab="Steps",fill=I("forestgreen"),col=I('navy'))
```

### Q: What is the average daily activity pattern?

```{r, echo=TRUE}
## Plot of 5 Minute Intervals
mean.pattern=aggregate(steps~interval,data = dataset, FUN=mean)
ggplot(data=mean.pattern,aes(x=interval,y=steps))+geom_line(size=1.5,aes(colour=I("forestgreen")))+labs(title="Average Steps/5 Minute Interval")
## Interval with maximum number of steps:
mean.pattern[mean.pattern[,2]==max(mean.pattern[,2]),]
```

### Q: Imputing Missing Values

```{r,echo=TRUE}
## Create new column for average/interval
new.dataset=merge(dataset,mean.pattern,by="interval")
names(new.dataset)=c("interval","steps","date","avg steps")
## If Steps is Missing, Fill with Avg Steps from Interval
for(i in 1:nrow(new.dataset)){
  
if(is.na(new.dataset[i,"steps"])) {
  new.dataset[i,"steps"]=new.dataset[i,"avg steps"]
} else{
  new.dataset[i,"steps"]=new.dataset[i,"steps"]
} 
}
new.dataset=new.dataset[,-4]
## Create new variable for mean steps/interval
mean.day.2=aggregate(steps~date,data=new.dataset,FUN=sum)
## Create new histogram
qplot(mean.day.2$steps,geom="histogram",bins=25,main="Histogram of Steps/Day",xlab="Steps",fill=I("forestgreen"),col=I('navy'))
## New Mean
mean(mean.day.2[,2])
## New Median
median(mean.day.2[,2])
```
The mean remains the same but the median is now the same value as the mean since we have replaced all missing values with the means from that interval.

Inputing the missing data increased the overall number of steps per day.


### Are there differences in activity patterns between weekdays and weekends?

First we need to create a new factor variable for weekend/weekdays

```{r}
for(i in 1:nrow(new.dataset)){
  
if(weekdays(new.dataset[i,"date"])=="Saturday"| weekdays(new.dataset[i,"date"])=="Sunday") {
  new.dataset[i,"day"]="weekend"
} else{
  new.dataset[i,"day"]="weekday"
} 
}

## Then turn the day variable into a factor and split into two data frames
new.dataset[,"day"]=as.factor(new.dataset[,"day"])
day.data=split(new.dataset,new.dataset$day)
wkdays=day.data[[1]]
wkends=day.data[[2]]
## Set New Variables for Charting
wkdays.pattern=aggregate(steps~interval,data = wkdays, FUN=mean)
wkends.pattern=aggregate(steps~interval,data = wkends, FUN=mean)

## Plot Weekend and Weekdays Average Steps/5 Minute Intervals

plot.wkdays=ggplot(data=wkdays.pattern,aes(x=interval,y=steps))+geom_line(size=1.5,aes(colour=I("forestgreen")))+labs(title="Weekdays Average Steps/5 Minute Interval")
plot.wkends=ggplot(data=wkends.pattern,aes(x=interval,y=steps))+geom_line(size=1.5,aes(colour=I("forestgreen")))+labs(title="Weekends Average Steps/5 Minute Interval")
grid.arrange(plot.wkdays,plot.wkends)
```
