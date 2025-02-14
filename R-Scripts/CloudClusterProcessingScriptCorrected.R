library(dplyr)
library(data.table)
library(lubridate)
library(openair)


## Read in Cloud Water Chemistry Data ####

CloudData<-read.csv("AllCloudandMetData.csv", header = TRUE)
CloudData<-CloudData%>%
  filter(Year > 2013)%>%
  mutate(date = as.POSIXct(date, format = "%m/%d/%Y %H:%M"))%>%
  mutate(date = as.character(date))%>%
  mutate(CloudDate = date) ## Setting cloud date is important for trajectories
CloudData$LWC<-NULL

## Read in WFM Met Data from Lance Lab and ALSC Tenures (back to 2014) ####
## Fread is important for speed and RAM effiency

AWIMet<-fread("AWIMetData.csv")
AWIMet21<-fread("MetData2021.csv")
#AWIMet22<-fread("Met_Data_2022.csv") ## If you want to expand the analysis 
ALSCMet<-fread("WFC12HourMet.csv")

## Select same columns. ####
## Use data.table when possible to save time and memory
AWIMet<-AWIMet[, date := as.POSIXct(date, format = "%m/%d/%Y %H:%M")][,
              .(date, LWC, SR, Temperature, RainCount, CloudCount)]
  
AWIMet21<-AWIMet21[, date := as.POSIXct(date, format = "%m/%d/%Y %H:%M")][,
      .(date, LWC, SR, Temperature, RainCount, CloudCount)]

#AWIMet22<-AWIMet22[, date := as.POSIXct(paste0(date, Time), format = "%d/%m/%Y %H:%M")][,
 #                 .(date, LWC, SR, Temperature, RainCount, CloudCount)]

AWIMet<-rbind(AWIMet, AWIMet21) ## Bind data together
#AWIMet<-rbind(AWIMet, AWIMet22)

ALSCMet<-ALSCMet[, date := as.POSIXct(date, format= "%m/%d/%Y %H:%M")]
  ## Convert ALSC date to same format

  
   ## Convert data to hourly
##Convert AWI Met to hourly data, remove non-cloud LWC values#### 
AWIMetHour<-AWIMet[,date :=floor_date(date, unit = 'hours')][,## Converts dates to hourly 
  LWC :=.(ifelse(LWC < 0.05 | LWC > 2.75, NA, LWC))][,
  lapply(.SD, mean),by = date]


## Combine All Met Data Together ####

ALSC<-ALSCMet[,.(date, LWC, SR, Temperature, RainCount, CloudCount)]
AWI<-AWIMetHour[,.(date,LWC, SR, Temperature, RainCount, CloudCount)]
AllMet<-rbind(ALSC,AWI)
AllMet<-AllMet[,date :=as.character(date)]

rm(AWIMet21)
#rm(AWIMet22)
rm(AWI)
rm(AWIMet)
rm(AWIMetHour)
rm(ALSC)
rm(ALSCMet)

## Read in TrajData ####

TrajAll<-fread("NAMTrajAll.csv", header = TRUE)

TrajAll<-TrajAll[, date:= as.POSIXct(date, format ="%Y-%m-%d %H:%M")][
  , hour.inc := hour_along][, date := as.character(date)]


AllData<-as.data.table(right_join(CloudData, AllMet, by = 'date'))
AllData<-AllData[order(date)][,
                              date := ymd_hms(date)
][,CloudDate := ymd_hms(CloudDate)][,CloudDate:=nafill(CloudDate, type = "nocb")][,date := as.character(date)]

ColumnList<-c("Year", "Month","LABPH","SPCOND", "CA", "MG", "Sodium", "K",
              "NH4", "SO4", "NO3", "CL", "WSOC","LWCNew", "CO", "lat","lon",
              "height","lat_i","lon_i","pressure")


CombinedTraj<-as.data.table(right_join(TrajAll, AllData, by ="date"))
CombinedTraj<-CombinedTraj[, CloudDate:=nafill(CloudDate, type = "nocb")][,
    (ColumnList):= lapply(.SD, function(x) nafill(x, type = 'nocb')), .SDcols = ColumnList]


## Find the Std Dev of Trajs by Cloud Date

#CombinedTraj<-CombinedTraj
#CombinedTraj<-CombinedTraj[,`:=`(StdLat = sd(lat, na.rm =TRUE), StdLon = sd(lon, na.rm = TRUE),
 #                                MeanLat = mean(lat, na.rm = TRUE), MeanLon = mean(lon, na.rm = TRUE)),
  #                         by = .(CloudDate,hour_along)][,
   #                        `:=`(CVLat = MeanLat/StdLat, CVLon = MeanLon/StdLon)][,
    #                        `:=`(MeanLatDiff = lat - MeanLat, MeanLonDiff = lon - MeanLon)][,
     #                       `:=` (AvgDiffLat = mean(abs(MeanLatDiff), na.rm = TRUE),
      #                            AvgDiffLon = mean(abs(MeanLonDiff), na.rm = TRUE)),by = .(traj_dt_i)]
## Don't use for PFAS paper!!!!
#CombinedTrajReduced<-CombinedTraj[AvgDiffLat < 3 & AvgDiffLon < 8]
CombinedTrajReduced<-CombinedTraj

## Check what fraction of data is kept ####

nrow(CombinedTrajReduced%>%filter(hour.inc==0 & Year < 2022))/nrow(CombinedTraj%>%filter(hour.inc==0 & Year <2022))

## Get an Average
CombinedTrajMean<-CombinedTraj[,lapply(.SD,mean), by = .(CloudDate, hour.inc), .SDcols = ColumnList
                                      ][,date:=.(as.POSIXct(CloudDate))]



#filter(CombinedTraj, CloudDate == as.POSIXct('2014-06-16 18:00:00'))
## Cluster Analysis Using All Trajectories ####

#AllClusterAnalysis<-trajCluster(CombinedTraj%>%
 #                                 filter(Year< 2022)%>%
  #                                mutate(date = as.POSIXct(date)), n.cluster = 7,
   #                             cols ='Set2',
    #                            origin = TRUE, key.pos = "top", key.col =4, lwd =6,
     #                           fontsize = 18)
AllClusterMean<-trajCluster(CombinedTrajMean%>%
                              filter(Year < 2022)%>%
                              mutate(date = as.POSIXct(date)), n.cluster = 7,
                            cols ='Set2',
                            origin = TRUE, key.pos = "top", key.col =4, lwd =6,
                            fontsize = 18)

ClusterOutputData<-AllClusterMean$data$results ## Cluster Traj Data
#fwrite(ClusterOutputData, "NAMTrajClusters.csv")


