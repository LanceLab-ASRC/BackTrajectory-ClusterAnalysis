library(scales)
library(tidyverse)
library(lubridate)
library(vroom)
library(tibbletime)
library(data.table)

#setwd("")

###
##Read in Cloud Water Data ####
CloudData<-fread("AllCloudandMetData.csv", header = TRUE)
CloudData<-CloudData%>%
  mutate(date = as.POSIXct(date, format = "%m/%d/%Y %H:%M"))%>%
  mutate(CloudDate = date, date = as.character(date))
CloudData$LWC<-NULL
### Read in Met Data. Excel can't store the minute data for 2021, need a better file format in the future. For now we can keep them seperated
AWIMet<-fread("AWIMetData.csv")
AWIMet21<-fread("MetData2021.csv")
#AWIMet22<-fread('Met_Data_2022.csv')|>
 # mutate(date = as.POSIXct(paste0(date, Time), format = "%d/%m/%Y %H:%M"))


## Change variable names in ALSC and AWI data so I can bind them
AWIMet<-AWIMet%>%
  #parse_date_time(date, c("%m/%d/%Y %H:%M","%Y-%m-%d %H:%M:S"), tz = "America/New_York")
  mutate(date = as.POSIXct(date, format = "%m/%d/%Y %H:%M"))%>%
  select(c("date", "LWC", "SR", "Temperature", "RainCount", "CloudCount"))

AWIMet21<-AWIMet21%>%
  mutate(date = as.POSIXct(date, format = '%m/%d/%Y %H:%M'))%>%
  select(c("date", "LWC", "SR", "Temperature", "RainCount", "CloudCount"))

#AWIMet22<-AWIMet22%>%
 # mutate(date = as.POSIXct(date, format = '%d/%m/%Y %H:%M'))%>%
  #select(c("date", "LWC", "SR", "Temperature", "RainCount", "CloudCount"))


AWIMet<-rbind(AWIMet, AWIMet21)
#AWIMet<-rbind(AWIMet, AWIMet22)

AWIMet<-as_tbl_time(AWIMet, index = date)

## Create hourly met data using the end time. Unclear what ALSC did, but doesn't seem to matter a whole lot anyway.
AWIMetHour<-AWIMet%>%
  collapse_by(period = "hourly", side = 'end')%>%
  group_by(date)%>%
  mutate(LWC = ifelse(LWC < 0.05 | LWC > 2.75, NA, LWC),
         date = date+minutes(1))%>%
  summarise(across(where(is.numeric), mean, na.rm = TRUE))


##ALSC Data ####
ALSCMet<-read.csv("WFC12HourMet.csv", header = TRUE)
ALSCMet<-ALSCMet%>%
  mutate(date = as.POSIXct(date, format = "%m/%d/%Y %H:%M"))
##Select the same columns for both DFs combine into one file

### Grab the proper columns and order them so you can combine ALSC and AWI Data

ALSC<-ALSCMet%>%
  select(c("date", "LWC", "SR", "Temperature", "RainCount", "CloudCount"))
AWI<-AWIMetHour%>%
  select(c("date", "LWC", "SR", "Temperature", "RainCount", "CloudCount"))
AllMet<-rbind(ALSC,AWI)
AllMet<-AllMet%>%
  mutate(LWC = ifelse(LWC <= 0.05 , NA, LWC))%>%
  filter(!is.na(LWC))%>%
  mutate(date = as.character(date))

### Join the cloud data and Met data by cloud dates. Then fill the empty cloud dates
## Keeping only the dates that are with 12 hours of the cloud date (ie the times associated with the cloud sample)

AllData<-right_join(CloudData, AllMet, by = 'date')%>%
  arrange(date)%>%
  mutate(date =ymd_hms(date), CloudDate = ymd_hms(CloudDate))%>%
  fill(CloudDate,.direction = 'up')%>%
  mutate(DiffDay = day(CloudDate)- day(date), DiffHour = abs(hour(CloudDate)- hour(date)),
         DiffDate = abs(CloudDate-date))%>%
  filter(DiffDate<= 43200)

### Write the File. May not stick with the csv style. Database maybe?
write.csv(AllData, "HysplitMetData.csv", row.names = FALSE)



