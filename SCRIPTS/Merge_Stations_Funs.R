compute_Distances <- function(stations,crop_cycles,latStat_Nam = "Latitude",lonStat_Nam = "Longitude",latFld_Nam ="Latitude",
                              lonFld_Nam ="Longitude",Station_Name= 'Station_Name', crop_cycle_Name= 'crop_cycle_Name'){
  
  # Compute distance between stations and crop_cycles
  
  gd <- lapply(1:nrow(crop_cycles),function(z){
    
    cbind(sapply(1:nrow(stations),function(x){
      gdist(lon.1= crop_cycles[,lonFld_Nam][z], 
            lat.1 =crop_cycles[,latFld_Nam][z],
            lon.2 = stations[,lonStat_Nam][x],
            lat.2 = stations[,latStat_Nam][x],units = "m")} ))   
  }  )
  
  dist_Stat_crop_cycle <- do.call(rbind,lapply(1:length(gd),function(x){
    
    data.frame(ID_crop_cycle = crop_cycles[,crop_cycle_Name][x],
               Station  = stations[,Station_Name] , dist=gd[[x]])
  }
  
  )
  )
  
  distData1  <- dist_Stat_crop_cycle
  
  distData1$Station <- paste("est",dist_Stat_crop_cycle[,"Station"],sep=".")
  
  distData2 <- reshape(distData1,idvar='ID_crop_cycle',timevar = 'Station',direction = "wide")
  
  names(crop_cycles)[-1] <- paste0('crop_cycle_',names(crop_cycles)[-1])
  
  distDataM1 <- merge(dist_Stat_crop_cycle,crop_cycles,by.x="ID_crop_cycle",by.y = crop_cycle_Name, all.x = T,all.y =F )
  
  names(stations)[-1] <- paste0('Stat_',names(stations)[-1])
  
  distDataM2 <- merge(distDataM1,stations,
                      by.x= "Station" , by.y=Station_Name, all.x=T, all.y=F)
  
  distDataM2
  
  
}

distances_Rules <- function(dist_Stat_crop_cycles,Dtemp  = 30000,Drain  = 15000,Drhum  = 30000,Drads  = 30000,DifElv = 150) {
  
  # To apply rules to identify potential weather station that represent the weather conditions for each crop_cycle
  
  # Maximun distances (en meters)
  
  # Temperaturas maximas y minimas
  # Precipitacion
  # Humedad relativa
  # Energia o brillo solar
  
  
  dist_Stat_crop_cycles$difElevation <- abs(dist_Stat_crop_cycles$crop_cycle_Elevation - dist_Stat_crop_cycles$Stat_Elevation) 
  
  # Reglas
  
  tmaxRule <- dist_Stat_crop_cycles$Stat_TX==1 & (dist_Stat_crop_cycles$difElevation   <=  DifElv & dist_Stat_crop_cycles$dist < Dtemp)
  
  tminRule <- dist_Stat_crop_cycles$Stat_TM==1 & (dist_Stat_crop_cycles$difElevation   <= DifElv & dist_Stat_crop_cycles$dist < Dtemp)
  
  rainRule <- dist_Stat_crop_cycles$dist < Drain & (dist_Stat_crop_cycles$Stat_P==1) & (dist_Stat_crop_cycles$difElevation <= 500 )
  
  esolRule <- dist_Stat_crop_cycles$dist < Drads & (dist_Stat_crop_cycles$Stat_SR==1) & (dist_Stat_crop_cycles$difElevation <= 500 )
  
  rhumRule <- dist_Stat_crop_cycles$dist < Drhum & (dist_Stat_crop_cycles$Stat_RH==1) & (dist_Stat_crop_cycles$difElevation <= 500 )
  
  # Variable con disponibilidad de datos
  
  dist_Stat_crop_cycles$UsefullData <- ""  # creando vector nulo
  
  dist_Stat_crop_cycles$UsefullData[tmaxRule] <- "TX"
  
  dist_Stat_crop_cycles$UsefullData[tminRule] <- paste(dist_Stat_crop_cycles$UsefullData[tminRule],"TM",sep=",")
  
  dist_Stat_crop_cycles$UsefullData[rainRule] <- paste(dist_Stat_crop_cycles$UsefullData[rainRule],"P",sep=",") 
  
  dist_Stat_crop_cycles$UsefullData[esolRule] <- paste(dist_Stat_crop_cycles$UsefullData[esolRule],"SR",sep=",") 
  
  dist_Stat_crop_cycles$UsefullData[rhumRule] <- paste(dist_Stat_crop_cycles$UsefullData[rhumRule],"RH",sep=",")
  
  dist_Stat_crop_cycles$UsefullData <- sub("^,","", dist_Stat_crop_cycles$UsefullData)
  
  # Remover lotes con coincidencias vacias
  
  dist_Stat_crop_cycles <- dist_Stat_crop_cycles[dist_Stat_crop_cycles$UsefullData!="" ,]
  
  #Unificación de datos
  head(dist_Stat_crop_cycles)
  #setkey(dist_Stat_crop_cycles)
  
  usefullStation <- unique(dist_Stat_crop_cycles)
  
  names(usefullStation)
  
  #Opcional, ordenar variables
  
  usefullStation <- as.data.table(as.data.frame(usefullStation))
  
  setorder(usefullStation,ID_crop_cycle,dist,difElevation)
  
  usefullStation <- as.data.frame(usefullStation)
  
  StationsList <- ddply(usefullStation,~ Station + Stat_Elevation,summarise, Total_crop_cycles_coverage = length(Station) )
  
  list( usefullStation = usefullStation,StationsList = StationsList)
  
}  

merge_Stations <- function(Stations_target,crop_cycles,crop_cycle_Name= 'FID',Dtemp  = 30000,Drain  = 15000,
                           Drhum  = 30000, Drads  = 30000,DifElv = 150){
  
  dist_Stat_crop_cycles <- compute_Distances(Stations_target,crop_cycles,crop_cycle_Name= 'FID')
  #save(dist_Stat_crop_cycles,file='dist_Stat_crop_cycles.RData')
  dist_Stat_crop_cycles <- dist_Stat_crop_cycles[dist_Stat_crop_cycles$crop_cycle_Ini_Date >= dist_Stat_crop_cycles$Stat_Min_Date & dist_Stat_crop_cycles$crop_cycle_End_Date <=  dist_Stat_crop_cycles$Stat_Max_Date,]
  
  dist_Stat_crop_cycles <- dist_Stat_crop_cycles[complete.cases(dist_Stat_crop_cycles),]
  
  distances_Rules(dist_Stat_crop_cycles = dist_Stat_crop_cycles,Dtemp  = Dtemp,Drain  = Drain,
                  Drhum  = Drhum, Drads  = Drads,DifElv = DifElv)
  
}



assing_var_station <- function(crop_cycles,usefullStation,list_Stations_Unprocess){
  
  IQ_crop_cycle_Station <-
    do.call(rbind,
            lapply( seq(nrow(crop_cycles)) ,function(i){
              
              # print(paste0(i,'-'))
              
              crop_cycle <- crop_cycles[i,]
              
              subSet <- usefullStation[usefullStation$ID_crop_cycle ==   crop_cycle$FID,]
              
              MaxDist    <- max(subSet$dist)
              ElevMax    <- max(subSet$difElevation)
              LongEvent  <- as.numeric(crop_cycle$End_Date - (crop_cycle$Ini_Date-1))
              
              # Starting date and ending date in the available stations for this crop_cycle
              
              DateStart <- crop_cycle$Ini_Date
              DateEnd   <- crop_cycle$End_Date
              
              stationsAsociated <- as.character(unique(subSet$Station))
              
              # Quality index in each crop_cycle for each station
              
              QI_Final <-
                do.call(rbind,
                        lapply( seq(length(stationsAsociated)),function(stat){
                          # cat(stat,'-')
                          stat_crop_cycle <- subSet[stat,]
                          
                          station <- stationsAsociated[stat]
                          
                          namStatUnprocess <- names(list_Stations_Unprocess)
                          
                          Stat_Unprocess <- list_Stations_Unprocess[grep(station,namStatUnprocess,fixed = T)] # Estacion fuente no procesada
                          
                          #varsInters <- do.call(rbind,strsplit(names(Stat_Unprocess),'_'))[,2] 
                          varsInters <- unlist(strsplit(stat_crop_cycle$UsefullData,','))
                          
                          
                          QI <- data.frame(QI_SR=NA,QI_P=NA,QI_RH=NA,QI_TX=NA,QI_TM=NA)
                          
                          for(var in seq(length(varsInters))){ 
                            
                            varSelected <- varsInters[var]
                            
                            subSetStation <- dplyr::filter(Stat_Unprocess[[paste(station,varSelected,sep='_')]],
                                                           Date >= DateStart & Date <= DateEnd)
                            
                            
                            NAsCount          <-  sum(is.na(subSetStation[,2]))/LongEvent
                            
                            Distcrop_cyclestat     <-  stat_crop_cycle$dist/MaxDist
                            
                            Elevcrop_cyclestat     <-  stat_crop_cycle$difElevation/ElevMax
                            
                            # Updating each QI according the variable's presence 
                            
                            index_value <- 
                              switch(varSelected,
                                     TX={0.2*Distcrop_cyclestat + 0.5*Elevcrop_cyclestat + 0.3*NAsCount},
                                     TM={0.2*Distcrop_cyclestat + 0.5*Elevcrop_cyclestat + 0.3*NAsCount},
                                     SR={0.5*Distcrop_cyclestat + 0.2*Elevcrop_cyclestat + 0.3*NAsCount},
                                     RH={0.3*Distcrop_cyclestat + 0.3*Elevcrop_cyclestat + 0.4*NAsCount},
                                     P= {0.5*Distcrop_cyclestat + 0.2*Elevcrop_cyclestat + 0.3*NAsCount},
                                     {NA})
                            
                            QI[paste0('QI_',varSelected)] <- index_value
                          }
                          data.frame(dist = stat_crop_cycle$dist,difElevation=stat_crop_cycle$difElevation,QI)
                        } 
                        )
                )
              
              data.frame(FID=crop_cycle$FID,Station = stationsAsociated,QI_Final,Ini_Date=crop_cycle$Ini_Date,End_Date=crop_cycle$End_Date)
            }
            )
    )  
  
  countsAllIQsNA <- apply(IQ_crop_cycle_Station[,grep("QI",names(IQ_crop_cycle_Station))],1,function(g){sum(is.na(g))}) # Contar las columnas de QI con NA
  
  IQ_crop_cycle_Station <- IQ_crop_cycle_Station[countsAllIQsNA<5,] # Remover filas que tengan todo en NA
  
  IQ_crop_cycle_Station_split <- split(IQ_crop_cycle_Station,IQ_crop_cycle_Station$FID) # 
  
  do.call(rbind,lapply(seq(length(IQ_crop_cycle_Station_split)),function(z){
    # cat(z,"-")
    
    crop_cycle_stat <- IQ_crop_cycle_Station_split[[z]]
    
    nam_crop_cycle_stat <- names(crop_cycle_stat)
    
    IQ_VARS <- grep('QI',nam_crop_cycle_stat)
    
    vars <- do.call(rbind,strsplit(nam_crop_cycle_stat[IQ_VARS],'_'))[,2]
    
    Station = crop_cycle_stat$Station[ sapply( crop_cycle_stat[,IQ_VARS],
                                               function(w){
                                                 if(sum(is.na(w))==nrow(crop_cycle_stat)){
                                                   NA
                                                 }else{which.min(w)}
                                               })]
    
    
    df <- data.frame( crop_cycle =  unique(crop_cycle_stat$FID) , Variable = vars,Station = Station,
                      Ini_Date=crop_cycle_stat$Ini_Date[1],End_Date=crop_cycle_stat$End_Date[1])
    
    df[complete.cases(df),]
  }))
}

weather_assignation <- function(assignation = crop_cycle_vars_station_assignation,listStat_Var ){
  
  assignation$Station <- as.character(assignation$Station )
  
  listEvent <- split(assignation,assignation$crop_cycle)
  
  nomFecSim <- "Ini_Date"
  nomFecCos <- "End_Date" 
  nomDateStat <- "Date"
  
  
  climDiar <- lapply(listEvent, 
                     function(x){
                       
                       u <- x[1,]
                       date <- as.Date(u[,nomFecSim]:u[,nomFecCos], origin="1970-01-01")
                       climaDiario <- data.frame(date)
                       
                       for(var in x$Variable){
                         
                         myDate <- data.frame(listStat_Var[[u$Station]][nomDateStat],listStat_Var[[u$Station]][var])
                         
                         climaDiario <- merge(climaDiario,myDate,by.x="date",by.y = "Date",all.x = T,all.y = F)
                       }
                       
                       climaDiarioEvent <- climaDiario
                       
                       climaDiarioEvent
                     }
  )
  
  climDiar
}


weather_indicators <- function( Final_assignation,vars = c('TX','TM','SR','RH','P'),
                                Indicadores  = c("mean(TX)","mean(TM)","mean((TX+TM)/2)",
                                                                    "mean(TX-TM)","sum(SR)","sum(TX>34)/length(TX)",
                                                                    "sum(P)","sum(P > 10)/length(P)",
                                                                    "sum(TM<15)/length(TM)","mean(RH)","sd(RH)" ),
                                
                                namFun = c("TX_Avg", "TM_Avg","T_Avg","DR_Avg",
                                           "SR","TX_34_Fq","P_Ac","P_10_Fq",
                                           "TM_15_Fq","RH_Avg","RH_sd" )){
  
Final_assignation <- lapply( Final_assignation,function(w){
  
  novar <- vars[!(vars %in% names(w))]
  
  if(length(novar)>0){
    segme <- as.data.frame(matrix(NA,nrow =nrow(w) ,ncol = length(novar)))
    names(segme) <- novar
     w <- cbind(w,segme)
  }
  w
 }
)
  weather_indicator <-
    as.data.frame(
      t(
        sapply(Final_assignation,function(event){
          with(event,
               sapply(Indicadores,function(y){eval(parse(text=y))})
          )
        }))
    )
  names(weather_indicator) <- namFun
  
  row.names(weather_indicator) <- names(Final_assignation)
  
  data.frame( crop_cycle =  row.names(weather_indicator),weather_indicator)
}


