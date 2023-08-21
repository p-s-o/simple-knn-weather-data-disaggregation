rm(list = ls())

for (dnr in 1:2){

#vars = 1 #T. P, R, W, S   #select weather data for knn (T=temperature, P=precipitation, R=rel. humidity, W=wind speed, S=radiation)
#vars = 2 #T, P, R
#vars = 3 #T, P
#vars = 4 #T
#vars = 5 #P

#example: different time scales for database and knn timeseries
if (dnr == 1) {dir = "choose folder for results"; timescale = 1; vars = 3}
if (dnr == 2) {dir = "choose folder for results"; timescale = 2; vars = 3}

#timescale 1
if(timescale == 1){
  years = 2003:2014 #total timescale
  years_dat = 2003:2008  #database timeseries
  years_knn = 2009:2014} #knn timeseries

#timescale 2
if(timescale == 2){
  years = 2003:2014 #total timescale
  years_dat = 2009:2014  #database timeseries
  years_knn = 2003:2008} #knn timeseries
  
#read in weather stations for knn
setwd("dir with list of stations")
stations = read.csv("list_of_stations.csv",sep=",")
stations = stations$x #header of col
stations = stations[1:31] # list of stations

#read in station data
read = function(var,s){
  if (vartype == 1){
    x = read.csv(paste("...path to station data...",s,"_",var,".csv",sep=""),header=T)
    x["grid"] = x$valgrid}
  return(x)}


for (s in stations){ #start of station loop
  timevec_d = read.table("...path to timevec_d.../timevec_d.csv",sep=",",header = F) #read in time vector - daily values
  names(timevec_d)[1:5] = c("y","m","d","h","date")
  timevec_d = filter(timevec_d, y %in% years)
  len = length(timevec_d$y)
  
  #read in available weather station data
  var_t = read("T",s) #temperature
  var_p = read("P",s) #precipitation
  var_r = read("R",s) #rel. humidity
  var_w = read("W",s) #wind speed
  var_s = read("S",s) #radiation
  
  #sort out values outside of 2003-2014 for each variable
  var_t = filter(var_t, y %in% years)
  var_p = filter(var_p, y %in% years)
  var_r = filter(var_r, y %in% years)
  var_w = filter(var_w, y %in% years)
  var_s = filter(var_s, y %in% years)
  
  #knn-dataframe for each station, used to find matchdays for knn-days
  knn = as.data.frame(matrix(NA,nrow=len,ncol=21))
  names(knn)[1:21] = c("y","m","d","date","T0","T1","P0","P1","R0","R1","W0","W1","S0","S1","DT","DP","DR","DW","DS","DG","matchday")
  knn$y = timevec_d$y
  knn$m = timevec_d$m           #knn$T0/P0/R0/W0/S0 = station values 
  knn$d = timevec_d$d           #knn$T1/P1/R1/W1/S1 = grid-value
  knn$date = timevec_d$date
  knn$T0 = var_t$valgrid
  knn$P0 = var_p$valgrid
  knn$R0 = var_r$valgrid
  knn$W0 = var_w$valgrid
  knn$S0 = var_s$valgrid
  knn = filter(knn, y %in% years_knn)
  len_knn = length(knn$y)
  
  #create database dataframe for each day to compare to knn-dataframe to find matchday
  database = as.data.frame(matrix(NA,nrow=len,ncol=25))
  names(database)[1:25] = c("y","m","d","date","DG","T0","T1","T2","DT","P0","P1","P2","DP","R0","R1","R2","DR","W0","W1","W2","DW","S0","S1","S2","DS")
  database$y = timevec_d$y
  database$m = timevec_d$m           #database$T0/P0/R0/W0/S0 = grid-value
  database$d = timevec_d$d           #database$Z1/P1/R1/W1/S1 = station value for which a matchday is needed
  database$date = timevec_d$date     #database$T2/P2/R2/W2/S2 = original grid-values
  
  database$T0 = var_t$valgrid
  database$P0 = var_p$valgrid
  database$R0 = var_r$valgrid #looking for matchday for these values
  database$W0 = var_w$valgrid
  database$S0 = var_s$valgrid
  
  database$T2 = var_t$grid 
  database$P2 = var_p$grid
  database$R2 = var_r$grid #original grid-values
  database$W2 = var_w$grid
  database$S2 = var_s$grid
  
  database = filter(database, y %in% years_dat)
  
  #day loop
  for (d in 1:len_knn){
    #temp-dataframe for each day to find matchday
    temp = datenbank
    #only values of the same month
    month = as.numeric(knn$m[d])
    
    #filter for occurance of precipitation
    if (dnr %in% c(25,26,27,28)) {
      if (is.na(knn$P0[d]) == F) {  
        if (knn$P0[d] == 0) {
          temp = filter(temp, P0 == 0)
        } else if (knn$P0[d] > 0) {
          temp = filter(temp, P0 > 0)
        }
      }
    }
    
    temp = filter(temp, m == month)
    
    #distance calculation for each variable
    if (is.na(knn$T0[d]) == F) { #vars %in% c(1,2,3,4)
      temp$T1 = knn$T0[d]
      temp$DT = temp$T0 - temp$T1
      temp$DT = abs(temp$DT)
      ran = max(temp$DT,na.rm=T) - min(temp$DT,na.rm=T)
      temp$DT = temp$DT/ran
    }
    
    if (is.na(knn$P0[d]) == F) { #vars %in% c(1,2,3,5)
      temp$P1 = knn$P0[d]
      temp$DP = temp$P0 - temp$P1
      temp$DP = abs(temp$DP)
      ran = max(temp$DP,na.rm=T) - min(temp$DP,na.rm=T)
      if (ran != 0){
        temp$DP = temp$DP/ran
      }
    }
    
    if (is.na(knn$R0[d]) == F) { #vars %in% c(1,2)
      temp$R1 = knn$R0[d]
      temp$DR = temp$R0 - temp$R1
      temp$DR = abs(temp$DR)
      ran = max(temp$DR,na.rm=T) - min(temp$DR,na.rm=T)
      temp$DR = temp$DR/ran
    }
    
    if (is.na(knn$W0[d]) == F) { #vars == 1
      temp$W1 = knn$W0[d]
      temp$DW = temp$W0 - temp$W1
      temp$DW = abs(temp$DW)
      ran = max(temp$DW,na.rm=T) - min(temp$DW,na.rm=T)
      temp$DW = temp$DW/ran
    }
    
    if (is.na(knn$S0[d]) == F) { #vars == 1
      temp$S1 = knn$S0[d]
      temp$DS = temp$S0 - temp$S1
      temp$DS = abs(temp$DS)
      ran = max(temp$DS,na.rm=T) - min(temp$DS,na.rm=T)
      temp$DS = temp$DS/ran
    }
    
    #calculate total distance
    for (t in 1:length(temp$y)){
      if (vars == 1 && is.na(temp$DT[t]) == F && is.na(temp$DP[t]) == F) {temp$DG[t] = sum(temp$DT[t],temp$DP[t],temp$DR[t],temp$DW[t],temp$DS[t],na.rm = T)}
      if (vars == 2 && is.na(temp$DT[t]) == F && is.na(temp$DP[t]) == F) {temp$DG[t] = sum(temp$DT[t],temp$DP[t],temp$DR[t],na.rm = T)}
      if (vars == 3 && is.na(temp$DT[t]) == F && is.na(temp$DP[t]) == F) {temp$DG[t] = sum(temp$DT[t],temp$DP[t])}
      if (vars == 4) {temp$DG[t] = temp$DT[t]}
      if (vars == 5) {temp$DG[t] = temp$DP[t]}
    }
    
    #sort temp by total distance
    temp = temp[!(is.na(temp$DG) == T),]
    temp = temp[order(temp$DG),]
    k = round(sqrt(length(temp$y)))
    temp = temp[1:k,]

    #transfer value to knn-dataframe of station
    knn$DG[d] = temp$DG[1]
    
    if (is.na(knn$DG[d]) == F){
      knn$T1[d] = temp$T2[1]
      knn$P1[d] = temp$P2[1]
      knn$R1[d] = temp$R2[1]
      knn$W1[d] = temp$W2[1]
      knn$S1[d] = temp$S2[1]}
    
    knn$DT[d] = temp$DT[1]
    knn$DP[d] = temp$DP[1]
    knn$DR[d] = temp$DR[1]
    knn$DW[d] = temp$DW[1]
    knn$DS[d] = temp$DS[1]
    
    if (is.na(knn$DG[d]) == T)
      {knn$matchday[d] = NA} else
      {knn$matchday[d] = temp$date[1]}
  } #end of day loop

  #grid and knn values in one dataframe
  dat = knn[knn$y %in% years_knn,c("y","m","d","date")]
  dat["t_grid"] = var_t$grid[var_t$y %in% years_knn]
  dat["p_grid"] = var_p$grid[var_p$y %in% years_knn]
  dat["r_grid"] = var_r$grid[var_r$y %in% years_knn]
  dat["w_grid"] = var_w$grid[var_w$y %in% years_knn]
  dat["s_grid"] = var_s$grid[var_s$y %in% years_knn]
  dat["t_neu"] = knn$T1
  dat["p_neu"] = knn$P1
  dat["r_neu"] = knn$R1
  dat["w_neu"] = knn$W1
  dat["s_neu"] = knn$S1
  
  write.table(dat,paste(dir,"/results_",s,".csv",sep=""),col.names = T,row.names = T,sep = ",")   #print knn results of current stations

  print(paste(s,"- done"))
} #end of station loop

} #end of variation loop