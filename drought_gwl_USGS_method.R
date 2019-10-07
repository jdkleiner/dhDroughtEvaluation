#THIS SCRIPT CALCULATES THE CURRENT "Depth to water level, feet below land surface (Maximum) level" FOR ALL USGS DROUGHT WELLS
#THE CORRESPONDING PERCENTILE IS THEN CALCULATED USING THE HISTORIC CONTINUOUS AND PERIODIC "Depth to water level" VALUES FOR THE CURRENT MONTH
#   STORED VIA REST:
#     drought_status_well (PROPERTY ON Well FEATURE THAT IS UPDATED EACH DAY THE SCRIPT IS RUN)
#     gwl_7day_ft (TIMESERIES ON Well FEATURE THAT IS CREATED EACH DAY THE SCRIPT IS RUN)
#     nonex_pct (PROPERTY ON TIMESERIES ABOVE THAT IS CREATED EACH DAY THE SCRIPT IS RUN)
#
#----------------------------------------------------------------------------------------------------------
rm(list = ls())  #clear variables
#library(waterData) #https://cran.r-project.org/web/packages/waterData/waterData.pdf
library(dataRetrieval) #https://cran.r-project.org/web/packages/dataRetrieval/dataRetrieval.pdf
require(data.table)
require(zoo)
library(httr)
library(lubridate) #required for year()
library(doBy) #required for summaryBy()


#SERVER:
#source("/var/www/R/config.local.private"); 
#LOCAL:
source("C:/Users/nrf46657/Desktop/VAHydro Development/GitHub/hydro-tools/config.local.private");

# load libraries
source(paste(vahydro_directory,"rest_functions.R", sep = "/")); 
source(paste(hydro_tools,"auth.private", sep = "/"));#load rest username and password, contained in auth.private file
token <- rest_token (base_url, token, rest_uname = rest_uname, rest_pw = rest_pw) #token needed for REST
site <- base_url


#Pull in list of all drought USGS well dH Features 
URL <- paste(site,"drought-wells-export", sep = "/");
well_list <- read.table(URL,header = TRUE, sep = ",")
hydrocodes <- well_list$hydrocode

#j <-1
#j <-18

#Begin loop to run through each USGS gage 
for (j in 1:length(hydrocodes)) {
  siteNumber <- hydrocodes[j]
  siteNumber <- toString(siteNumber)
  print(paste("USGS siteNumber: ", siteNumber, sep='')); 
  siteNumber = unlist(strsplit(siteNumber, split='_', fixed=TRUE))[2]
  
  #welldata <- whatNWISdata(siteNumber, service = "all", parameterCd = "all",statCd = "all")
  welldata <- whatNWISdata(siteNumber = siteNumber)#, service = "all", parm_cd = "all",stat_cd = "all")
  
  #Parameter code '72019' = Depth to water level, feet below land surface (ft) https://help.waterdata.usgs.gov/code/parameter_cd_nm_query?parm_nm_cd=%25level%25&fmt=html
  gwl_row <- which(welldata$parm_cd == 72019)
  gwl_rows <- welldata[gwl_row,]
  gwl_rows <- gwl_rows[ order(gwl_rows$begin_date , decreasing = TRUE ),]
  begin_date_row <- gwl_rows[length(gwl_rows$begin_date),]
  begin_date <- begin_date_row$begin_date
  print(paste("Historic Record begining ",begin_date,sep=""))
  
  url <- paste("https://waterdata.usgs.gov/nwis/dv?cb_72019=on&format=rdb_meas&site_no=",siteNumber,"&referred_module=sw&period=&begin_date=",begin_date,"&end_date=",sep="")
  print(paste("Retrieving Data from NWIS using:",url))
  data <- read.table(url,header = TRUE, sep = "\t")
  
  print(head(data))
  
  data$max <- as.numeric(as.character(data[,5])) #COPY MAX (Depth to water level) COLUMN AND FORCE 'NA's WHERE THERE IS MISSING DATA
  data$periodic <- as.numeric(as.character(data[,9])) #CREATE COLUMN OF PERIODIC MEASUREMENTS

  #most recent max gwl reading 
  latest_row <- length(data$max)
  latest_row <- data[latest_row,]
  gw_lvl <- latest_row$max
  gw_lvl <- as.numeric(as.character(gw_lvl)) 
  #need to handle 'EDT' for most recent value (use the day before)
  if  (is.na(gw_lvl) == TRUE) {
    latest_row <- length(data$max)
    latest_row <- latest_row - 1
    latest_row <- data[latest_row,]
    gw_lvl <- latest_row$max
    gw_lvl <- as.numeric(as.character(gw_lvl))
  }
  print(gw_lvl) #print most recent daily mean gwl reading 

  #Create dataframe of all month's names and numeric values
  months <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
  months_num <- c('-01-','-02-','-03-','-04-','-05-','-06-','-07-','-08-','-09-','-10-','-11-','-12-')
  months_all <- data.frame(months,months_num)
  
  #Determine Current month name and numeric value
  month_row <- which(months_all$months == format(Sys.time(),"%b"))
  month_num <- months_all[month_row,]
  month <- month_num$months
  month <- toString(month)
  month_num <- month_num$months_num
  
  #Determine historic percentiles for current month using continuous and periodic measurments
  data [ grep( month_num , data$datetime, perl= TRUE ), "month" ] <- month
  month_rows <- which(data$month == month)
  month_data <- data[month_rows,] #ISOLATE CURRENT MONTH'S DATA

  #ISOLATE CONTINUOUS DATA FOR CURRENT MONTH
  continuous_data <- month_data[!(is.na(month_data$max) | month_data$max==""), ]
  continuous_data$gwl_value <- continuous_data$max

  #ISOLATE PERIODIC DATA FOR CURRENT MONTH
  periodic_data <- month_data[!(is.na(month_data$periodic) | month_data$periodic==""), ]
  periodic_data$gwl_value <- periodic_data$periodic
   
  #COMBINE CONTINUOUS AND PERIODIC DATA FOR CURRENT MONTH
  month_data_all <- rbind(periodic_data,continuous_data)
   
  #ADD YEAR COLUMN AND CALCULATE MEDIAN VALUE FOR EACH YEAR
  month_data_all$year <- year(month_data_all[,"datetime"])
  month_data_medians <- summaryBy(gwl_value ~ year, data = month_data_all, FUN = list(median))
   
  #REMOVE CURRENT YEAR MEDIAN VALUE - CURRENT YEAR WILL NOT BE USED FOR CALCULATING HISTORIC PERCENTILES
  month_data_medians <- month_data_medians[-length(month_data_medians[,1]),]

  #CREATE VECTOR OF MEDIANS
  gwl_medians <- as.numeric(as.character(month_data_medians$gwl_value.median))
  #gwl_medians <- round(gwl_medians,2)
   
  #CALCULATE HISTORIC PERCENTILES FROM MEDIANS
  quant_num <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
  quant <- c(0, 0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95, 1)
  month_quant <- quantile(gwl_medians, probs =  quant) 
  month_quant <- round(month_quant,2)
  
  print(month_quant)
  print(gw_lvl) 

  
  #gw_lvl <-9
  #i<-1
  
  
  #CALCULATE TODAY'S PERCENTILE BASED ON HISTORIC PERCENTIILES
  gw_lvl_round <- round(as.numeric(as.character(gw_lvl)), digits=7)
  if ((gw_lvl_round < round(as.numeric(as.character(month_quant[9])), digits=7)) == 'FALSE') {   #IF RECORD HIGH groundwater level
    rolling_percentile <- 100
  } else if ((gw_lvl_round > round(as.numeric(as.character(month_quant[1])), digits=7)) == 'FALSE') {   #IF RECORD LOW groundwater level
    rolling_percentile <- 0
  } else {  #BETWEEN HISTORIC LOW AND HISTORIC HIGH
    
    for (i in 1:length(quant_num)) {  
      
      lower_quantile <- quant_num[i]
      upper_quantile <- quant_num[i]+1
      lower_percent <- quant[i]
      upper_percent <- quant[i+1]
      
      if ((gw_lvl >= month_quant[lower_quantile] && gw_lvl <= month_quant[upper_quantile] ) == 'TRUE') {
        rolling_percentile <- lower_percent + (gw_lvl- month_quant[lower_quantile])*(( upper_percent - lower_percent)/(month_quant[upper_quantile]-month_quant[lower_quantile]))
        rolling_percentile <- as.vector(rolling_percentile)
        rolling_percentile <- rolling_percentile*100
      } 
    }
  }
  rolling_percentile <- (100 - rolling_percentile)
  
  print(paste("The gw_lvl for today is ",gw_lvl,sep=""))
  print(paste("This is equivalent to a percentile of ",rolling_percentile," for the month of ",month," at GAGE ",siteNumber,sep=""))
  
  #Determine the drought status based on percentile thresholds 
  if ((rolling_percentile  > 25) == 'TRUE') {nonex_propcode <- 0}
  if ((rolling_percentile  >= 10 && rolling_percentile <= 25) == 'TRUE') {nonex_propcode <- 1}
  if ((rolling_percentile >= 5 && rolling_percentile  < 10) == 'TRUE') {nonex_propcode <- 2}
  if ((rolling_percentile  < 5) == 'TRUE') {nonex_propcode <- 3}
  print(paste("The Drought Status Propcode for ",siteNumber," is ",nonex_propcode,sep=""))
  
  
  #################################################################################################
  #--STORE WITH REST 'drought_status_well', 'gwl_7day_ft', and 'nonex_pct'
  #################################################################################################
  
  #Set values to dH variables
  gwl_7day_ft <- gw_lvl
  nonex_propcode <- as.numeric(as.character(nonex_propcode))
  nonex_propvalue <- round(as.numeric(as.character(rolling_percentile)),2)
  
  #Convert USGS well No. to dH Hydrocode 
  hydrocode <- paste("usgs_",siteNumber, sep="")
  
  #Retrieve dH usgsgage feature from vahydro
  well_inputs <- list(hydrocode=hydrocode)
  well_feature <- getFeature(well_inputs,token,base_url)
  hydroid <- as.character(well_feature$hydroid[1])
  
  
      #------CREATE/UPDATE 'gwl_7day_ft' TIMESERIES    
       tsbody = list(
         featureid = hydroid,
         varkey = 'gwl_7day_ft',
         entity_type = 'dh_feature',
         tsvalue = gwl_7day_ft,
         tstime = as.numeric(as.POSIXct(Sys.Date()-6,origin = "1970-01-01", tz = "GMT")),
         tsendtime = as.numeric(as.POSIXct(Sys.Date(),origin = "1970-01-01", tz = "GMT")),
         tscode = NULL
       );
      
      post_ts <- postTimeseries(tsbody, base_url)
      tid <- post_ts
      
      #------CREATE/UPDATE 'nonex_pct' PROPERTY ATTACHED TO 'gwl_7day_ft' TIMESERIES   
      pbody = list(
        bundle = 'dh_properties',
        featureid = tid,
        varkey = 'nonex_pct',
        entity_type = 'dh_timeseries',
        propname = 'nonex_pct',
        propvalue = nonex_propvalue,
        propcode = nonex_propcode
      );
      
      post_prop <- postProperty(inputs = pbody, base_url = base_url)
      
      
      #------UPDATE 'drought_status_well' PROPERTY ATTACHED TO WELL 
      status_pbody = list(
        bundle = 'dh_properties',
        featureid = hydroid,
        varkey = 'drought_status_well',
        entity_type = 'dh_feature',
        propname = 'drought_status_well'
      );
      get_status_prop <- getProperty(inputs = status_pbody, base_url = base_url)

      status_pbody$pid <- get_status_prop$pid
      status_pbody$propvalue <- nonex_propvalue
      status_pbody$propcode <- nonex_propcode
  
      post_status_prop <- postProperty(inputs = status_pbody, base_url = base_url)

} #end of well feature loop

