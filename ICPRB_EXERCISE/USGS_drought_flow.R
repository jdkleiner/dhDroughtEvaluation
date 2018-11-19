#THIS SCRIPT CALCULATES THE CURRENT 7-DAY RUNNING AVERAGE "Depth to water level, feet below land surface (Maximum) level" FOR ALL USGS DROUGHT GAGES 
#THE CORRESPONDING PERCENTILE IS THEN CALCULATED USING THE HISTORIC "DAILY" "Depth to water level" VALUES FOR THE CURRENT MONTH
#   STORED VIA REST:
#     drought_status_stream (PROPERTY ON GAGE FEATURE THAT IS UPDATED EACH DAY THE SCRIPT IS RUN)
#     q_7day_cfs (TIMESERIES ON GAGE FEATURE THAT IS CREATED EACH DAY THE SCRIPT IS RUN)
#     nonex_pct (PROPERTY ON TIMESERIES ABOVE THAT IS CREATED EACH DAY THE SCRIPT IS RUN)
#
#----------------------------------------------------------------------------------------------------------
rm(list = ls())  #clear variables
#library(waterData)
library(dataRetrieval)
require(data.table)
require(zoo)
library(httr)
library(stringr)

# source("/var/www/R/config.local.private"); 
# # load libraries
# source(paste(vahydro_directory,"rest_functions.R", sep = "/")); 
# source(paste(hydro_tools,"auth.private", sep = "/"));#load rest username and password, contained in auth.private file
# token <- rest_token (base_url, token, rest_uname = rest_uname, rest_pw = rest_pw) #token needed for REST
# site <- base_url


#https://cran.r-project.org/web/packages/waterData/waterData.pdf
#https://cran.r-project.org/web/packages/dataRetrieval/dataRetrieval.pdf

#Pull in list of all drought USGS gage dH Features 
#URL <- paste(site,"ows-cova-usgs-drought-site-list-export", sep = "/");
#URL <- paste(site,"drought-gages-export", sep = "/");
#gagelist <- read.table(URL,header = TRUE, sep = ",")
#hydrocode <- gagelist$hydrocode
#USGS_GAGES <- str_split_fixed(gagelist$hydrocode, "usgs_", 2)
#gagelist$USGS_GAGES <- USGS_GAGES[,2]
#USGS_GAGES <- gagelist$USGS_GAGES 


daily <- read.csv("C:/Users/nrf46657/Desktop/OWS/ICPRB/Drought Exercise 2018/VA_DroughtIndicators_streamflow.csv", header = TRUE)

USGS_GAGES <- c("01634000","01654000")

# if (USGS_GAGES == "01634000") {
#   daily <- data.frame(Date = daily$Date, Flow = daily$X01634000)
#   } else if (USGS_GAGES == "01654000"){
#   daily <- data.frame(Date = daily$Date, Flow = daily$X01654000)
#   }





#j<-10
#j<-9
#j<-6
j <- 1
#Begin loop to run through each USGS gage 
for (j in 1:length(USGS_GAGES)) {
  USGS_GAGE_ID <- USGS_GAGES[j]
  print(paste("USGS_GAGE_ID ", USGS_GAGE_ID, sep='')); 

  
  
  if (USGS_GAGE_ID == "01634000") {
    daily_gage <- data.frame(Date = daily$Date, Flow = daily$X01634000)
  } else if (USGS_GAGE_ID == "01654000"){
    daily_gage <- data.frame(Date = daily$Date, Flow = daily$X01654000)
  } 
  
  
  
  
  
#--RETRIEVE FLOW DATA AND CALCULATE 'drought_status_stream' and 'q_7day_cfs'

#Retrieve all historic gage streamflow data in cfs
# Using waterData:
#gage_info <- siteInfo(USGS_GAGE_ID)
# Using dataRetrieval
gage_info <- readNWISsite(USGS_GAGE_ID)

# Using waterData:
#staid <- gage_info$staid
# Using dataRetrieval
staid <- gage_info$site_no

# Using waterData:
#staname <- gage_info$staname
# use dataRetrieval 
staname <- gage_info$station_nm
# Using waterData:
#gage <- importDVs(USGS_GAGE_ID)
# use dataRetrieval 


#############################################################################
# Retrieve Historic Gage Data From NWIS 
############################################################################
gage <- readNWISdv(USGS_GAGE_ID,'00060')
gage <- renameNWISColumns(gage)
############################################################################
############################################################################
# Retrieve Flow Data From CSV File
############################################################################


#gage_01634000 <- data.frame(gage$Date, gage$X01634000)
#gage_01654000 <- data.frame(gage$Date, gage$X01654000)

############################################################################
############################################################################

#Calculate 7-Day Average Streamflow for every day of the historic record
rollmean_7day <- rollmeanr(gage$Flow,7,fill=NA)
gage <- cbind(gage, rollmean_7day )
#tail(gage)

############################################################################
############################################################################
daily_rollmean_7day <- rollmeanr(daily_gage$Flow,7,fill=NA)
daily_gage <- cbind(daily_gage, daily_rollmean_7day)
############################################################################
############################################################################



#--Current ROLLING 7-DAY AVERAGE for USGS Streamgage
#latest_row <- gage[length(gage$Date),]
#rolling_7day_avg <- latest_row$rollmean_7day

#--Current ROLLING 7-DAY AVERAGE for USGS Streamgage
#latest_row <- daily_gage[length(daily_gage$Date),]
#rolling_7day_avg <- latest_row$daily_rollmean_7day



#Create dataframe of all month's names and numeric values
months <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
months_num <- c('-01-','-02-','-03-','-04-','-05-','-06-','-07-','-08-','-09-','-10-','-11-','-12-')
months_all <- data.frame(months,months_num)

# #Determine Current month name and numeric value
#   month_row <- which(months_all$months == format(Sys.time(),"%b"))
#   month_num <- months_all[month_row,]
#   month <- month_num$months
#   month <- toString(month)
#   month_num <- month_num$months_num
# 
# #Determine The percentiles for this month based on historic 7-Day Average Streamflows
#   gage [ grep( month_num , gage $Date, perl= TRUE ), "month" ] <- month
#   month_rows <- which(gage$month == month)
#   month_data <- gage[month_rows,]
#   month_data <- na.omit(month_data) #in case the first month of historic record is same as current month
#   #month_flows <- month_data$val
#   month_flows_7day <- month_data$rollmean_7day
#   quant_num <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
#   quant <- c(0, 0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95, 1)
#   month_quant <- quantile(month_flows_7day, probs =  quant) 

  
 ########################################################################################### 
 # COMPARE DAILY GAGE DATA TO HISTORIC QUANTILES 
 ###########################################################################################  
drought.df <- data.frame(Date=as.Date(character()),
                         Flow=character(), 
                         daily_rollmean_7day=character(), 
                         percentile=character(), 
                         drought_status=character(), 
                         stringsAsFactors=FALSE) 



  #z <- 1 
  for (z in 1:length(daily_gage$daily_rollmean_7day)) { 
  
    print(paste("Processing Day ",z," of ",length(daily_gage$daily_rollmean_7day),sep=''))
    
  #Iteration z of ROLLING 7-DAY AVERAGE for flow timeseries 
   daily_rollmean <- daily_gage[z,]$daily_rollmean_7day
   daily_month <- format(as.Date(daily_gage[z,]$Date),"%b")
   
   
   #if(is.na(daily_rollmean)) next 
   if(is.na(daily_rollmean)) {daily_rollmean <- 999999}
   
   
   #Determine Current month name and numeric value
   month_row <- which(months_all$months == daily_month)
   month_num <- months_all[month_row,]
   month <- month_num$months
   month <- toString(month)
   month_num <- month_num$months_num
   
   #Determine The percentiles for this month based on historic 7-Day Average Streamflows
   gage [ grep( month_num , gage $Date, perl= TRUE ), "month" ] <- month
   month_rows <- which(gage$month == month)
   month_data <- gage[month_rows,]
   month_data <- na.omit(month_data) #in case the first month of historic record is same as current month
   #month_flows <- month_data$val
   month_flows_7day <- month_data$rollmean_7day
   quant_num <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
   quant <- c(0, 0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95, 1)
   month_quant <- quantile(month_flows_7day, probs =  quant) 
    
    
  if ((daily_rollmean < as.numeric(as.character(month_quant[9]))) == 'FALSE') {   #IF RECORD LOW
    rolling_percentile <- 100
  } else if ((daily_rollmean > as.numeric(as.character(month_quant[1]))) == 'FALSE') {   #IF RECORD HIGH
    rolling_percentile <- 0
  } else if ((daily_rollmean < as.numeric(as.character(month_quant[1])) && daily_rollmean > as.numeric(as.character(month_quant[9]))) == 'FALSE') {  #BETWEEN HISTORIC LOW AND HISTORIC HIGH
    
  for (i in 1:length(quant_num)) {  

    lower_quantile <- quant_num[i]
    upper_quantile <- quant_num[i]+1
    lower_percent <- quant[i]
    upper_percent <- quant[i+1]
    
        if ((daily_rollmean >= month_quant[lower_quantile] && daily_rollmean <= month_quant[upper_quantile] ) == 'TRUE') {
         rolling_percentile <- lower_percent + (daily_rollmean- month_quant[lower_quantile])*(( upper_percent - lower_percent)/(month_quant[upper_quantile]-month_quant[lower_quantile]))
         rolling_percentile <- as.vector(rolling_percentile)
         rolling_percentile <- rolling_percentile*100
        }
        }
  }
print(paste("The Rolling 7-day Avg for z is ",daily_rollmean,sep=""))
print(paste("This is equivalent to a percentile of ",rolling_percentile," for the month of ",month," at GAGE ",USGS_GAGE_ID,sep=""))

#Determine the drought status based on percentile thresholds 
if ((rolling_percentile  > 25) == 'TRUE') {nonex_propcode <- 0}
if ((rolling_percentile  >= 10 && rolling_percentile <= 25) == 'TRUE') {nonex_propcode <- 1}
if ((rolling_percentile >= 5 && rolling_percentile  < 10) == 'TRUE') {nonex_propcode <- 2}
if ((rolling_percentile  < 5) == 'TRUE') {nonex_propcode <- 3}
print(paste("The Drought Status Propcode for ",USGS_GAGE_ID," is ",nonex_propcode,sep=""))

drought_status <- nonex_propcode
drought.row <- cbind(daily_gage[z,], rolling_percentile, drought_status)


drought.df <- rbind(drought.df,drought.row)

}

write.csv(drought.df, file = paste(USGS_GAGE_ID,'_daily_status.csv',sep=""))



} #end of gage feature loop

