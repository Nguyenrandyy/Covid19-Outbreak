# Randy Nguyen  
# Data Visualization Project
# https://github.com/nytimes/covid-19-data
UScases_B <- read.csv("~/Downloads/us.csv",as.is = TRUE)
View(UScases_B)

head(UScases_B)
dim(UScases_B)
tail(UScases_B)
#Each row of the data corresponds to one day. The first column shows the data. 
#The second column showsthe total number of COVID-19 cases up to that day. 
#The third column shows the total number of deaths upto that day. 
#There are total of 370 days starting 21 January 2020 and ending 24 January 2021

summary(UScases_B)
#The data is in order by date. Thus, the first quartile of case numbers will represent the number of cases one quarter of the way from the beginning of the pandemic until now. The first quartile in cases is 847,780. If we examine the data, we see that 847,780 cases occurred between the 92nd and 93rd days of the data. Day 92.5 (=370/4) is one quarter of the way through the dataset (after accounting for rounding). Likewise, the median case number of 4,086,798 occurs between days 185 and 186, which is half way through.

library(tidyverse)
UScases <- read.csv("~/Downloads/us.csv",as.is = TRUE)
UScases_T <- as_tibble(UScases)
UScases_T

##This plot is The Number of cases versus the date
par(mfrow=c(1,1))
plot(as.Date(UScases_B[,1]),UScases_B[,2],type="l",xlab="date",ylab="number of cases", main="US Cases")
#This plot of The Number of cases versus the date

#This plot represents the number of cases from the start of January 21, 2020 - March 1, 2020
firstday=41
lastday=370
par(mfrow=c(1,1))
plot(as.Date(UScases_B[firstday:lastday,1]),UScases_B[firstday:lastday,2],type="l",xlab="date",ylab="number of cases")

#This plot represents the number of deaths from January 21, 2020 - March 1, 2020.
firstday=41
lastday=370
par(mfrow=c(1,1))
plot(as.Date(UScases_B[firstday:lastday,1]),UScases_B[firstday:lastday,3],type="l",xlab="date",ylab="number of case")

#Plots of number of cases and deaths side-by-side
firstday=41
lastday=370
par(mfrow=c(1,2))
plot(as.Date(UScases_B[firstday:lastday,1]),UScases_B[firstday:lastday,2],type="l",xlab="date",ylab="number of cases", main="US Cases")
plot(as.Date(UScases_B[firstday:lastday,1]),UScases_B[firstday:lastday,3],type="l",xlab="date",ylab="number of deaths", main = "US Deaths")

#Side-by-side plots showing US daily cases and daily deaths from March 2020 - January 2021
firstday=41
lastday=370
dailyCases=diff(UScases_B[,2]) #daily cases.
dailyCases=c(1,dailyCases) 
dailyDeaths=diff(UScases_B[,3])
dailyDeaths=c(0,dailyDeaths)
UScases_B=data.frame(UScases_B,dailyCases,dailyDeaths)
par(mfrow=c(1,2))
plot(as.Date(UScases_B[firstday:lastday,1]),dailyCases[firstday:lastday],type="l",xlab="date",ylab="number of cases", main="US daily Cases")
plot(as.Date(UScases_B[firstday:lastday,1]),dailyDeaths[firstday:lastday],type="l",xlab="date",ylab="number of deaths", main ="US daily deaths")

firstday=41
lastday=370
avgTime=14
library(zoo)
par(mfrow=c(2,1))
plot(as.Date(UScases_B[firstday:lastday,1]),dailyCases[firstday:lastday],type="l",xlab="date",ylab="number of cases", main="US daily Cases")
lines(as.Date(UScases_B[(firstday+(avgTime-1)):lastday,1]),rollmean(dailyCases[firstday:lastday],k=14),col="red")
plot(as.Date(UScases_B[firstday:lastday,1]),dailyDeaths[firstday:lastday],type="l",xlab="date",ylab="number of cases", main="US daily Deaths")
lines(as.Date(UScases_B[(firstday+(avgTime-1)):lastday,1]),rollmean(dailyDeaths[firstday:lastday],k=14))

ggplot(data = UScases_B) + labs(title = "US cases", x = "date", y="number of cases") +
        geom_point(mapping=aes(x=as.Date(UScases$date), y=as.numeric(UScases$cases)))

ggplot(data = UScases_B) + labs(title = "US deaths", x = "date", y="number of deaths") +
        geom_point(mapping=aes(x=as.Date(UScases$date), y=as.numeric(UScases$deaths)))
