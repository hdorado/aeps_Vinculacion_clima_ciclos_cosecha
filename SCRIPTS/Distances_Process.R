

# Hugo Andres Dorado B.
# 09-04-2018

rm(list = ls())

library(here)
library(data.table)
library(plyr)
library(Imap)
library(pgirmess)


source(here::here('SCRIPTS','Merge_Stations_Funs.R'))

load(here::here('RESULTS','Catalogs.RData'))

load(here::here('BASIC_FILES','list_Station_Unprocess.RData'))

load(here::here('BASIC_FILES','list_Stations_Process.RData'))

# Filter stations per availability

earlist_date_crop_cycle <- min(crop_cycles$Ini_Date)
lastest_data_crop_cycle <- max(crop_cycles$End_Date) 


rule1 <- earlist_date_crop_cycle > Stations_catalog$Max_Date
rule2 <- lastest_data_crop_cycle < Stations_catalog$Min_Date

Stations_target <- Stations_catalog[!(rule1|rule2),]

# Filter station per cropping cycles

crop_cycles <- crop_cycles[min(Stations_target$Min_Date) < crop_cycles$Ini_Date,]

# Merge the station and cropping cycles according distances rules that are specified in the function,
# the time period between station - cropping cycles is validated as well, station and crop_cycles which
# don't

merg_Station <- merge_Stations(Stations_target,crop_cycles,crop_cycle_Name= 'FID',
              Dtemp  = 30000, Drain  = 15000,
              Drhum  = 30000, Drads  = 30000,
              DifElv = 150)

# Extract the usefullStation list

usefullStation <- merg_Station$usefullStation

# Selecting only fileds which have avaliable stations

crop_cycles <- crop_cycles[crop_cycles$FID %in% usefullStation$ID_crop_cycle,]

# Assigning each variable for each station

crop_cycle_vars_station_assignation <- assing_var_station(crop_cycles,usefullStation)

# Assigning daily weather information for each event variable

Final_weather_assign <- weather_assignation(crop_cycle_vars_station_assignation,list_Stations_Process)

# Generating weather indicators

Final_indicators <- weather_indicators(Final_weather_assign)

# Files to save

save( Final_weather_assign , file=here::here("RESULTS","Finalds_weather_assign.RData"))

write.csv(crop_cycle_vars_station_assignation,here::here("RESULTS","crop_cycle_vars_station_assignation.csv"),row.names = F)

write.csv(Final_indicators,here::here("RESULTS","Final_indicators.csv"),row.names = F)



