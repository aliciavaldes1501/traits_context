library(plyr)

#Reading data
loggers<-read.table("./data/clean/raw_data_loggers.txt",header=T,sep="\t",dec=".")
head(loggers)
str(loggers)

#Put the date in the correct format
loggers$date<-as.Date(loggers$date,format="%m/%d/%Y")
head(loggers)

#Leave only data from 16/07-31/08
loggers<-subset(loggers,date<"2015-09-01")

#Categorize day and night
loggers$timeday<-ifelse(loggers$hour==1|loggers$hour==4|loggers$hour==22,yes="night",no="day")

#Leave only daily temperatures
loggers_day<-subset(loggers,timeday=="day")

#Calculate daily mean, maximum, minimum, standard deviation and range
#For each id_pl and date (day temperatures)
loggers_day_agg<-ddply(loggers_day,c("pop", "id_pl","date"), summarise,
                       meanT = mean(LogData),
                       maxT = max(LogData),
                       minT = min(LogData),
                       sdT  = sd(LogData))
loggers_day_agg$rangeT<-loggers_day_agg$maxT-loggers_day_agg$minT
head(loggers_day_agg)
#Average for each id_pl (day temperatures)
loggers_day_agg<-aggregate(cbind(meanT, maxT, minT, sdT, rangeT) ~ pop + id_pl, data=loggers_day_agg, FUN=mean)
head(loggers_day_agg)
str(loggers_day_agg)

#Correlation matrix for the different temperature measures
cor(loggers_day_agg[3:7])

