library(scales)
library(tidyverse)
library(lubridate)
library(vroom)
library(tibbletime)
library(data.table)



# setwd("")


##   Read in Hourly LWC  ##
HourlyLWC <- fread("HourlyLWC_Vis_2009-2025.txt", header = TRUE)
HourlyLWC <- HourlyLWC %>%
  mutate(LWC_Start = as.POSIXct(Date_Time, format = "%m/%d/%Y %H:%M")) %>% ## Ensure DateTime Format
  mutate(LWC_Start = LWC_Start, LWC_Start = as.character(LWC_Start)) %>%
  select(LWC_Start, LWC_Merged) ## We only need the LWC start time and the merged column.
## NOTE: Times in HourlyLWC represent the start of the hour that LWC data are averaged from!!  ##

## Read in Average LWC per CW Sample ##
InCloudAvg <- fread("In-CloudAVGLWC_forEachCWSample.txt", header = TRUE)
InCloudAvg <- InCloudAvg %>%
  mutate(DumpTime = as.POSIXct(Dump_DateTime, format = "%m/%d/%Y %H:%M") )%>% ## Ensure DateTime Format
  mutate(DumpTime = DumpTime, DumpTime = as.character(DumpTime)) %>%
  rename(`Hours In-Cloud` = NumHrs) ## Simple name change to better reflect what is in the column.

## Merge Hourly LWC and Average LWC to Associate In-Cloud Hours with SampleID and Dump Time ##
CloudData <- InCloudAvg %>%
  mutate(Target = DumpTime, ## Make a Target for Sample DumpTime
         `Sample Year` = year(Target), ## ID Sample Year
         `Res (hr)` = if_else(`Sample Year` >= 2014, 12, 3)) %>% ## Note Sample Resolution based on Year
  rowwise() %>%
  mutate( ## Select Hourly LWC times for Corresponding DumpTime Targets
    HourlyRows = list({
    all_rows <- HourlyLWC %>% 
      filter(LWC_Start < Target) ## LWC Times are the start of measurement. DumpTimes are the end of the measurement.
    resolution <- if_else(`Sample Year` >= 2014, 12, 3) ## Define sample resolution based on year. 
    tail(all_rows, resolution) ## Select rows based on resolution.
  }),
  
  ## Average Hourly LWC for values > 0.05
  ## This is only done to ensure values match LWC_Merged from the canon.
  ## If values don't match, there is an error somewhere in forming "CloudData".
  MergedAverage = mean(HourlyRows$LWC_Merged[HourlyRows$LWC_Merged > 0.05], na.rm = TRUE)) %>%
  ## --- ##
  
  unnest(HourlyRows) %>%
  ungroup()

## Calculate Resolution Ratio...
CloudData <- CloudData %>%
  mutate(`Percent (%)` = (100* (`Hours In-Cloud` / `Res (hr)`)))

## !!!!!!!!!!!!!!!!!!!!!!
## If all checks are clear, multiple extraneous columns can be removed from the final data.
extraneous <- c("Dump_DateTime", "Target", "MergedAverage")
CloudData[extraneous] <- NULL
## ---- ##

## Clean up column order for simplicity...
CloudData <- CloudData %>%
  select(SampleID, DumpTime, LWC_Start, `Sample Year`, `Res (hr)`, `Hours In-Cloud`, `Percent (%)`, LWC_Merged, LWC_avg) %>%
  rename(`Hourly LWC` = LWC_Merged)

## Save/Export Data
write.csv(CloudData, "InCloudLWC2009-2025.csv", row.names = FALSE) 


