rm(list = ls())  #clear variables
#library(waterData)
library(dataRetrieval)
require(data.table)
require(zoo)
library(stringr)


USGS_GAGES <- c("01634000","01654000")

#j <- 1
#Begin loop to run through each USGS gage 
for (j in 1:length(USGS_GAGES)) {
  USGS_GAGE_ID <- USGS_GAGES[j]
  print(paste("USGS_GAGE_ID ", USGS_GAGE_ID, sep='')); 

gage_info <- readNWISsite(USGS_GAGE_ID)
staid <- gage_info$site_no
staname <- gage_info$station_nm
print(paste("Processing ",staname,sep=""))


#############################################################################
# Retrieve Historic Gage Data From NWIS 
############################################################################
gage <- readNWISdv(USGS_GAGE_ID,'00060')
gage <- renameNWISColumns(gage)
############################################################################
############################################################################

#Calculate 7-Day Average Streamflow for every day of the historic record
rollmean_7day <- rollmeanr(gage$Flow,7,fill=NA)
gage <- cbind(gage, rollmean_7day )
#tail(gage)

months <- c('Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')
months_num <- c('-01-','-02-','-03-','-04-','-05-','-06-','-07-','-08-','-09-','-10-','-11-','-12-')
months_all <- data.frame(months,months_num)

drought.df <- data.frame(Month=character(), 
                         Percentile_0 =character(), 
                         Percentile_5 =character(), 
                         Percentile_10 =character(), 
                         Percentile_25 =character(), 
                         Percentile_50 =character(), 
                         Percentile_75 =character(), 
                         Percentile_90 =character(), 
                         Percentile_95 =character(), 
                         Percentile_100 =character(), 
                         stringsAsFactors=FALSE) 

#x <- 1
for (x in 1:length(months)) { 
daily_month <- months[x]

print(paste("Processing ",daily_month,sep=""))

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

percentiles <- round(month_quant, digits = 2)
month_percentiles.row <- cbind(Month = month, t(percentiles))

colnames(month_percentiles.row)[colnames(month_percentiles.row)=="0%"] <- "Percentile_0"
colnames(month_percentiles.row)[colnames(month_percentiles.row)=="5%"] <- "Percentile_5"
colnames(month_percentiles.row)[colnames(month_percentiles.row)=="10%"] <- "Percentile_10"
colnames(month_percentiles.row)[colnames(month_percentiles.row)=="25%"] <- "Percentile_25"
colnames(month_percentiles.row)[colnames(month_percentiles.row)=="50%"] <- "Percentile_50"
colnames(month_percentiles.row)[colnames(month_percentiles.row)=="75%"] <- "Percentile_75"
colnames(month_percentiles.row)[colnames(month_percentiles.row)=="90%"] <- "Percentile_90"
colnames(month_percentiles.row)[colnames(month_percentiles.row)=="95%"] <- "Percentile_95"
colnames(month_percentiles.row)[colnames(month_percentiles.row)=="100%"] <- "Percentile_100"

drought.df <- rbind(drought.df,month_percentiles.row)
 

} 

startdate <- gage[1,]$Date
enddate <- gage[length(gage$Date),]$Date



write.csv(drought.df, file = paste(USGS_GAGE_ID,'_monthly_percentiles_',startdate,'__',enddate,'.csv',sep=""))


} #end of gage feature loop
