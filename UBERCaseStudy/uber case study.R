##Loading required libraries
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)


############################
##reading uber data and making stringAsFactors false as dates types not required as factors
uber<-read.csv("Uber Request Data.csv", stringsAsFactors = FALSE)
############################


############################
#view the uber dataset
View(uber)

##Observation:
## 1. There are NAs in the driver.id column and drop.timeStamp column. Since these are valid NAs since the request
##    status is "no cars available", hence no drivers were assigned and also there is no drop.timestamp as the 
##    trip never started. So we don't need to remove or handle these NAs
## 2. The request.timestamp and drop.timestamp have few variations in the date format. The date formats that we can 
##    see are: i) "DD/MM/YYYY HH:MM", ii) "DD/MM/YYYY HH:MM:SS", iii) "DD-MM-YYYY HH:MM:SS"
## So we will need to convert these to a single date format so as to convert them into date objects.
## 3. Also we can see that there is no case-sentive issue with any of the columns in the data.
############################


############################
##convert "/" in dates to "-", so that all dates are of a single format
uber$Request.timestamp <- str_replace_all(uber$Request.timestamp, "/", "-")
uber$Drop.timestamp <- str_replace_all(uber$Drop.timestamp, "/", "-")
############################


############################
##finding indices for all dates where time is present without secs. like "HH:MM"
##using regex to find such date strings
bad_format_request_time_index <- str_which(uber$Request.timestamp, pattern = "[:space:][:digit:]{1,2}:[:digit:]{1,2}$")
#adding ":00" to the end of all such strings with format "HH:MM"
uber$Request.timestamp[bad_format_request_time_index]<-paste(uber$Request.timestamp[bad_format_request_time_index], ":00",sep = "")

##finding indices for all dates where time is present without secs. like "HH:MM"
##using regex to find such date strings
bad_format_drop_time_index <- str_which(uber$Drop.timestamp, pattern = "[:space:][:digit:]{1,2}:[:digit:]{1,2}$")
#adding ":00" to the end of all such strings with format "HH:MM"
uber$Drop.timestamp[bad_format_drop_time_index]<-paste(uber$Drop.timestamp[bad_format_drop_time_index], ":00",sep = "")

##converting dates string columns to POSIX dates
uber$Request.timestamp<-as.POSIXct(uber$Request.timestamp, format="%d-%m-%Y %H:%M:%S")
uber$Drop.timestamp<-as.POSIXct(uber$Drop.timestamp, format="%d-%m-%Y %H:%M:%S")
#############################


#############################
##extracting derived column request hours from the request.timestamp column
uber$Request.hour<-format(uber$Request.timestamp, "%H")

typeof(uber$Request.hour)
#############################

##uber_original<-uber

#############################
##extracting derived column weekdays from request.timestamp
uber$weekday <- weekdays(uber$Request.timestamp)
#############################


#############################
##extracting driving time for each trip from drop.timestamp and request.timestamp
uber$totalDrivingTime <- round(as.numeric(uber$Drop.timestamp-uber$Request.timestamp), 2)
#############################


#############################
##plotting trips on different weekdays along with the request status types
ggplot(uber, aes(x=weekday, fill=Status))+geom_bar()

## we can see there is no pattern from the plot. The request are quite uniform throughtout each day.
## we can also see there are requests for Sunday and Saturday. This may be because it is not present in the dataset.
#############################


#############################
##Checking the driving time variance for different pickup.points
ggplot(uber, aes(y=totalDrivingTime, x=Pickup.point))+geom_boxplot(na.rm = TRUE)

## we can see the variance of the time taken for the trips are quite similar for each pick.point
## the median is also quite close for both the pickup.points
#############################


#############################
##plotting count of different trip status types in different colors as well
ggplot(uber, aes(x=Status, fill=Status))+
  geom_bar()+
  labs(x="Status", y="Total Count")+
  geom_text(stat="count",aes(label=..count..), vjust=-0.5)+
  ggtitle("Count of Requests per Status type")

## we can clearly see that there are high number requests for which there is no cars available as compared for the 
## requests which are getting cancelled
#############################


#############################
##lets check how is the distribution of request status types in various pickup.points 
ggplot(uber, aes(x=Status, fill=Pickup.point))+
  geom_bar(position = "dodge")+
  labs(x="Status", y="Total Count")+
  geom_text(stat="count",aes(label=..count..), vjust=-0.5, position = position_dodge(width = 1))+
  ggtitle("Distribution of Requests for Pickup Points")

##we can observe 2 things out of the plot clearly:
## 1. There are high number of "no cars Available" requests in The airport region.
## 2. There are high number of requests which are getting cancelled in the city region.
#############################


#############################
##let us now check the distribution of requests throughout the day (per hour)
## also lets split the status of request to see the distribution as well
ggplot(uber, aes(x=Request.hour,fill=Status))+
  geom_bar()+
  labs(x="Hours of the Day", y="Total Count")+
  geom_text(stat="count",aes(label=..count..), vjust=-0.5,position = position_stack())+
  ggtitle("Requests at different hours of the day")

## we can clearly see from the plot that in the morning from 05 hours to 09 hours there are high cancellation,
##and in the evening from 17 hours to 21 hours there are high "No cars available".
#############################


#############################
##from the above plot we can divide the hours into few time slots based on the number of requests
##creating a derived column on the intervals of request.hour column
uber <- mutate(uber, Request.timeSlot = ifelse( as.numeric(Request.hour) %in% 0:4 , "Late Night", 
                                                ifelse( as.numeric(Request.hour) %in% 5:9 , "Early Morning", 
                                                        ifelse( as.numeric(Request.hour) %in% 10:11 , "Late Morning", 
                                                                ifelse( as.numeric(Request.hour) %in% 12:16 , "After Noon", 
                                                                        ifelse( as.numeric(Request.hour) %in% 17:21 , "Evening", "Night"))))))

##Converting the time_slots into a ordered factor based on the request.hours
uber$Request.timeSlot <- factor(uber$Request.timeSlot, levels=unique(uber$Request.timeSlot[order(uber$Request.hour)]), ordered = TRUE)
##############################


##############################
##Creating a derived column called supply.
##Each request that is served i.e. the status is "Trip Completed" is a supply,
##while all the "Cancelled" and "No cars available" requests are considered as not supplied.
uber$Supply<-ifelse(uber$Status!="Trip Completed", yes = 0,no = 1)
uber$Supply<-as.factor(uber$Supply)
##############################


##############################
##lets check the supply and demand gap in various timeslots
## supply==1 is a valid supply, while Supply==0 is a gap i.e. could not be supplied
ggplot(uber, aes(x=Request.timeSlot,fill=Supply))+
  geom_bar()+
  labs(x="TIme Slots", y="Total Count")+
  geom_text(stat="count",aes(label=..count..), vjust=-0.5,position = position_stack())+
  ggtitle("Supply Demand Gap for Various Time Slots")

##We can clearly see that are 2 timeSlots which has a huge supply demand gap
## 1. Early Morning, 2. Evening
##############################


##############################
##Checking for which request types the supply demand gap is severe
ggplot(uber[uber$Supply==0,],aes(x=Request.timeSlot,fill=Pickup.point))+
  geom_bar(position = "dodge")+
  labs(x="TIme Slot", y="Total Count of Failed Requests")+
  geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = position_dodge(width = 1))+
  ggtitle("Supply Gap Comparison for Request Types (by Pickup.Point)")

##we can see in the evening at the airport there is a severe gap where supply could not be made
##1427 no supplies at the Airport in evening
##############################


##############################
##checking for early morning requests to why the gap is
early_morning_requests<- uber[uber$Request.timeSlot=="Early Morning",]

ggplot(early_morning_requests,aes(x=Status,fill=Pickup.point))+
  geom_bar(position = "dodge")+
  labs(x="Status", y="Total Count")+
  geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = position_dodge(width = 1))+
  ggtitle("Early Morning Requests")

## we see that high number of cabs are cancelled in the early morning at the city
## hence there is a supply demand gap
##############################


##############################
##checking for evening timeslots to see where the gap exists
evening_requests<- uber[uber$Request.timeSlot=="Evening",]

ggplot(evening_requests,aes(x=Status,fill=Pickup.point))+
  geom_bar(position = "dodge")+
  labs(x="Status", y="Total Count")+
  geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = position_dodge(width = 1))+
  ggtitle("Evening Requests")
##we see that there are high no of "No cars available" at airport
## hence a major supply demand gap is due to this
##############################


##############################
##lets check the comparison supply demand gap at both Airport and city 
ggplot(uber, aes(x=Request.timeSlot,fill=Supply))+
  geom_bar()+
  labs(x="Time Slot", y="Total Count")+
  geom_text(stat="count",aes(label=..count..), vjust=-0.5,position = position_stack())+
  facet_wrap(~Pickup.point)+
  ggtitle("Supply Demand Gap at Airport and City")

##############################




####Extra Plots to visualize

##############################
##Incoming cabs to Airport
ggplot(uber[uber$Pickup.point=="City" & uber$Status=="Trip Completed",],aes(x=Request.timeSlot))+
  geom_bar()+
  geom_text(stat = "count", aes(label=..count..), vjust=-0.5)+
  ggtitle("Incoming cabs to Airport at different timeslots")
##############################


##############################
##Total Request Generated at Airport
ggplot(uber[uber$Pickup.point=="Airport",],aes(x=Request.timeSlot))+
  geom_bar()+
  geom_text(stat = "count", aes(label=..count..), vjust=-0.5, position = position_stack())+
  ggtitle("Total Request Generated at the Airport")
##############################

##############################
ggplot(uber[uber$Status=="Trip Completed",], aes(x=Request.timeSlot,fill=Pickup.point))+
  geom_bar(position = "dodge")+
  geom_text(stat="count",aes(label=..count..), vjust=-0.5,position = position_dodge(width = 1))+ggtitle("Trip Completed")
##############################