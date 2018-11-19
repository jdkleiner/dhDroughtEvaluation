#THIS SCRIPT CALCULATES THE CURRENT 7-DAY RUNNING AVERAGE "Depth to water level, feet below land surface (Maximum) level" FOR ALL USGS DROUGHT GAGES 
#THE CORRESPONDING PERCENTILE IS THEN CALCULATED USING THE HISTORIC "DAILY" "Depth to water level" VALUES FOR THE CURRENT MONTH
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

# source("/var/www/R/config.local.private"); 
# # load libraries
# source(paste(vahydro_directory,"rest_functions.R", sep = "/")); 
# source(paste(hydro_tools,"auth.private", sep = "/"));#load rest username and password, contained in auth.private file
# token <- rest_token (base_url, token, rest_uname = rest_uname, rest_pw = rest_pw) #token needed for REST
# site <- base_url


# #Pull in list of all drought USGS well dH Features 
# URL <- paste(site,"drought-wells-export", sep = "/");
# well_list <- read.table(URL,header = TRUE, sep = ",")
# hydrocodes <- well_list$hydrocode


daily <- read.csv("C:/Users/nrf46657/Desktop/OWS/ICPRB/Drought Exercise 2018/VA_DroughtIndicators_groundwater.csv", header = TRUE)

#USGS_WELLS <- c("381002078094201","381132076551001","382150078424001","390348078035501","391542077423801")

#NO_GOOD -> "385253077042301"
USGS_WELLS <- "385253077042301"


#j <-1

#Begin loop to run through each USGS gage 
#for (j in 1:length(hydrocodes)) {
for (j in 1:length(USGS_WELLS)) {
  siteNumber <- USGS_WELLS[j]
  #siteNumber <- toString(siteNumber)
  print(paste("USGS siteNumber: ", siteNumber, sep='')); 
  #siteNumber = unlist(strsplit(siteNumber, split='_', fixed=TRUE))[2]

  
    daily_well <- data.frame(datetime = daily$Date, gwl = daily[,(1+j)])


  
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
data$min <- as.numeric(as.character(data[,7])) #COPY MIN (Depth to water level) COLUMN AND FORCE 'NA's WHERE THERE IS MISSING DATA
data$mean <- rowMeans(subset(data, select = c(max, min)), na.rm = TRUE) #CREATE COLUMN OF MEAN OF MAX AND MIN COLUMNS

#most recent gwl reading (mean of daily max and min depth to water levels)
# latest_row <- length(data$mean)
# latest_row <- data[latest_row,]
# gw_lvl <- latest_row$mean
# gw_lvl <- as.numeric(as.character(gw_lvl)) 
# #need to handle 'EDT' for most recent value (use the day before)
# if  (is.na(gw_lvl) == TRUE) {
#   latest_row <- length(data$mean)
#   latest_row <- latest_row - 1
#   latest_row <- data[latest_row,]
#   gw_lvl <- latest_row$mean
#   gw_lvl <- as.numeric(as.character(gw_lvl))
# }
# print(gw_lvl) #print most recent daily mean gwl reading 

rollmean_7day <- rollapply(data$mean, 7, mean, na.rm=TRUE)  #BUILD VECTOR OF 7-DAY ROLLING MEAN OF DAILY MEAN GWL
  rollmean_7day  <- append(rollmean_7day , NA, after = 0)   #ROLLING MEAN FUNCTION EXCLUDED FIRST 6 DATA VALUES
  rollmean_7day  <- append(rollmean_7day , NA, after = 0)     #MUST MANUALLY SET 'NA's FOR THE FIRST 6 VALUES
  rollmean_7day  <- append(rollmean_7day , NA, after = 0)
  rollmean_7day  <- append(rollmean_7day , NA, after = 0)
  rollmean_7day  <- append(rollmean_7day , NA, after = 0)
  rollmean_7day  <- append(rollmean_7day , NA, after = 0)
data$rollmean_7day <- rollmean_7day                         #CREATE COLUMN OF 7-DAY ROLLING MEANS ON DATAFRAME

#latest_row <- data[length(data$rollmean_7day),] #most recent 7-day rolling avg gwl
#gw_lvl <- latest_row$rollmean_7day
#print(gw_lvl) #print most recent 7-day rolling average daily mean gwl reading 


############################################################################
############################################################################
daily_rollmean_7day <- rollmeanr(daily_well$gwl,7,fill=NA)
daily_well <- cbind(daily_well, daily_rollmean_7day)
############################################################################
############################################################################




#Create dataframe of all month's names and numeric values
months <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
months_num <- c('-01-','-02-','-03-','-04-','-05-','-06-','-07-','-08-','-09-','-10-','-11-','-12-')
months_all <- data.frame(months,months_num)

#Determine Current month name and numeric value
# month_row <- which(months_all$months == format(Sys.time(),"%b"))
# month_num <- months_all[month_row,]
# month <- month_num$months
# month <- toString(month)
# month_num <- month_num$months_num

# #Determine The percentiles for this month based on 7-Day Average Streamflow
# data [ grep( month_num , data$datetime, perl= TRUE ), "month" ] <- month
# month_rows <- which(data$month == month)
# month_data <- data[month_rows,]
#write.csv(month_data, "month_data.csv")

#remove rows with emptys
# month_data <- month_data[!(is.na(month_data$rollmean_7day) | month_data$rollmean_7day==""), ]
# month_gwls <- month_data$rollmean_7day
# month_gwls <- as.numeric(as.character(month_gwls))
# #tail(month_gwls)
# 
# quant_num <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
# quant <- c(0, 0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95, 1)
# month_quant <- quantile(month_gwls, probs =  quant) 

#gw_lvl <-9
#i<-1


########################################################################################### 
# COMPARE DAILY WELL DATA TO HISTORIC QUANTILES 
###########################################################################################  
drought.df <- data.frame(Date=as.Date(character()),
                         gwl=character(), 
                         daily_rollmean_7day=character(), 
                         percentile=character(), 
                         drought_status=character(), 
                         stringsAsFactors=FALSE) 


#z <- 1 
for (z in 1:length(daily_well$daily_rollmean_7day)) { 

  print(paste("Processing Day ",z," of ",length(daily_well$daily_rollmean_7day),sep=''))
  
  #Iteration z of ROLLING 7-DAY AVERAGE for gwl timeseries 
  daily_rollmean <- daily_well[z,]$daily_rollmean_7day
  daily_month <- format(as.Date(daily_well[z,]$datetime),"%b")

  if(is.na(daily_rollmean)) {daily_rollmean <- 999999}

  #Determine Current month name and numeric value
  month_row <- which(months_all$months == daily_month)
  month_num <- months_all[month_row,]
  month <- month_num$months
  month <- toString(month)
  month_num <- month_num$months_num
  
  #Determine The percentiles for this month based on historic 7-Day Average Streamflows
  data [ grep(month_num, data$datetime, perl= TRUE ), "month" ] <- month
  month_rows <- which(data$month == month)
  month_data <- data[month_rows,]
  month_data <- na.omit(month_data) #in case the first month of historic record is same as current month
  #month_flows <- month_data$val
  month_flows_7day <- month_data$rollmean_7day
  quant_num <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
  quant <- c(0, 0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95, 1)
  month_quant <- quantile(month_flows_7day, probs =  quant) 
  
  
  
  daily_rollmean
  
#gw_lvl_round <- round(as.numeric(as.character(gw_lvl)), digits=7)
if ((daily_rollmean < round(as.numeric(as.character(month_quant[9])), digits=7)) == 'FALSE') {   #IF RECORD HIGH groundwater level
  rolling_percentile <- 100
} else if ((daily_rollmean > round(as.numeric(as.character(month_quant[1])), digits=7)) == 'FALSE') {   #IF RECORD LOW groundwater level
  rolling_percentile <- 0
  } else {  #BETWEEN HISTORIC LOW AND HISTORIC HIGH

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
rolling_percentile <- (100 - rolling_percentile)

print(paste("The gw_lvl for today is ",daily_rollmean,sep=""))
print(paste("This is equivalent to a percentile of ",rolling_percentile," for the month of ",month," at GAGE ",siteNumber,sep=""))

#Determine the drought status based on percentile thresholds 
if ((rolling_percentile  > 25) == 'TRUE') {nonex_propcode <- 0}
if ((rolling_percentile  >= 10 && rolling_percentile <= 25) == 'TRUE') {nonex_propcode <- 1}
if ((rolling_percentile >= 5 && rolling_percentile  < 10) == 'TRUE') {nonex_propcode <- 2}
if ((rolling_percentile  < 5) == 'TRUE') {nonex_propcode <- 3}
print(paste("The Drought Status Propcode for ",siteNumber," is ",nonex_propcode,sep=""))

drought_status <- nonex_propcode
drought.row <- cbind(daily_well[z,], rolling_percentile, drought_status)


drought.df <- rbind(drought.df,drought.row)

}

write.csv(drought.df, file = paste(siteNumber,'_daily_well_status.csv',sep=""))



} #end of gage feature loop