
# Genarete station
# Hugo Andres Dorado B.
# 09-04-2018

rm(list=ls())

library(here)
library(raster)
library(reshape2)

# Reading datasets

stations  <- read.csv(here::here('BASIC_FILES','estaciones_variables_fechas.csv'))

crop_cycles     <- read.csv(here::here('BASIC_FILES','cultivos_georefenciados.csv'))

elevation <- raster(here::here('BASIC_FILES','chiapas_srtm'))

# Date formate

stations$Ini_Date <- as.Date(stations$Ini_Date,'%m/%d/%Y')

stations$End_Date <- as.Date(stations$End_Date,'%m/%d/%Y')

crop_cycles$Ini_Date <- as.Date(crop_cycles$Ini_Date,'%m/%d/%Y')

crop_cycles$End_Date <- as.Date(crop_cycles$End_Date,'%m/%d/%Y')



# Elevation estimation

stations$Elevation <- extract(elevation ,stations[c('Longitude','Latitude')])

crop_cycles$Elevation    <- extract(elevation ,crop_cycles[c('Longitude','Latitude')])

# Remove stations or crop_cycles with missing values in elevation

stations <- stations[!is.na(stations$Elevation),] 

crop_cycles   <- crop_cycles[!is.na(crop_cycles$Elevation),]    

# Generation of the variables catalog 


station_groups_Date <- reshape(stations,v.names=c("Ini_Date","End_Date"),
                               idvar = c("Station_Name"),
                               timevar= "Variable",direction = "wide",sep = "_")

# Weather variables availability inspection

stations$availability <- 1

stations_availability  <- dcast(stations, Station_Name   ~  Variable,sum)
 
Stations_catalog <- merge(station_groups_Date,stations_availability,by='Station_Name',all=T,sort = F)


# Sorting the variables

nams_catalog <- names(Stations_catalog)

Stations_catalog$Min_Date <- apply(Stations_catalog[grep('Ini',nams_catalog)],1,min,na.rm=T)

Stations_catalog$Max_Date <- apply(Stations_catalog[grep('End',nams_catalog)],1,min,na.rm=T)

Stations_catalog <- Stations_catalog[,c(1:4,15:19,5:14,20:21)]

crop_cycles <- crop_cycles[,c(1:3,6,4:5)]

# Save outputs files

write.csv(Stations_catalog,here::here("RESULTS","Stations_catalog.csv"),row.names=F)

save(Stations_catalog,crop_cycles,file = here::here("RESULTS","Catalogs.RData"))


