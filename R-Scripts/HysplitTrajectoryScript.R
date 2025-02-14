library(dplyr)
library(purrr)
library(splitr)
library(lubridate)


#setwd("")
WFCMet<-read.csv("HysplitMetData.csv", header = TRUE) ## Read in data


HysplitFunction<-function(CloudWaterData, nyear){
  CloudWaterData<-CloudWaterData%>%
    mutate(date = as.POSIXct(date, format = "%Y-%m-%d %H:%M"))%>% ## Convert to data and create Day, yea, hour columns
    mutate(Day = day(date))%>%
    mutate(Year = year(date))%>%
    mutate(hourtime = hour(date))%>%
    filter(Year == nyear) ## Filter out data that doesn't occur on nyear
  #filter(Day == 25)%>% ## Allows for individual day runs
  # filter(month(date) == 8)|> ##Comment out when you want a full run
  dates = CloudWaterData$date # Grabs lists of dates from cloud water dataset
  dates = dates+hours(5) ##Converts into UTC
  print(dates)
  hourtime = hour(dates)
  df<-map2(dates, hourtime, function(x,y){ ## Similar to a for loop, iteriates through the lists of dates and hourtime, doing the function with variables x and y

      if(hour(x) > 21){ ### Only do this for NAM12 data. For whatever reason, NAM12 trajs need to be run 1 day past the last trajectory
       x<-c(x, x+days(1))
     }
    print(paste0("Datetime in UTC: ",x)) #Just tells you the progress of the code
    #print(y)
    Traj<-hysplit_trajectory( ## The HYSPLIT Function that was built in R
      lat = 44.3658,
      lon = -73.9028,
      height = 976,
      duration = 144,
      direction = "backward",
      met_type = "nam12",
      days = x,  ### Need to add 1 extra day to this list due to wierd problems with NAM 12 at 22 and 23 UTC
      extended_met = FALSE, ###If you wanted extra Met data, but its a lot of data.
      daily_hours = y,
    )
    # head(Traj)
    Traj<-Traj%>% ##Convert back to EST
      mutate(date = traj_dt_i-hours(5))%>%
      mutate(traj_dt_EST = traj_dt-hours(5))
    return(Traj)
    #  do.call(file.remove,list(list.files(pattern = "_nam12", full.names = FALSE)))
  })
  #  do.call(file.remove,list(list.files(pattern = "_nam12", full.names = FALSE))) ##Deletes Met data each month so you don't read in too much dataa
  All<-bind_rows(df) ##Convert from List to one dataframe. Not sure If this what I want to do.
  # do.call(file.remove,list(list.files(pattern = "_nam12", full.names = FALSE))) ##Deletes Met data each month so you don't read in too much dataa
  return(All)
}

#YearList<-seq(2014,2015, by = 1)
YearList<-c(2014, 2021) ## This is the list of how many years you want to run.


map(YearList, function(x){
  print(x)
  Traj<-HysplitFunction(WFCMet, x)
  #       print(head(Traj))
  do.call(file.remove, list(list.files(pattern = "nam"))) ## Deletes the HYSPLIT Met Data to prevent using up all memory
  write.csv(Traj, paste0('NAMTraj',x,".csv")) ## Writes the data to a csv
  #return(Traj)
})

