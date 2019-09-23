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

source("/var/www/R/config.local.private"); 
# load libraries
source(paste(vahydro_directory,"rest_functions.R", sep = "/")); 
source(paste(hydro_tools,"auth.private", sep = "/"));#load rest username and password, contained in auth.private file
token <- rest_token (base_url, token, rest_uname = rest_uname, rest_pw = rest_pw) #token needed for REST
site <- base_url


#https://cran.r-project.org/web/packages/waterData/waterData.pdf
#https://cran.r-project.org/web/packages/dataRetrieval/dataRetrieval.pdf

#Pull in list of all drought USGS gage dH Features 
#URL <- paste(site,"ows-cova-usgs-drought-site-list-export", sep = "/");
URL <- paste(site,"drought-gages-export", sep = "/");
gagelist <- read.table(URL,header = TRUE, sep = ",")
hydrocode <- gagelist$hydrocode
USGS_GAGES <- str_split_fixed(gagelist$hydrocode, "usgs_", 2)
gagelist$USGS_GAGES <- USGS_GAGES[,2]
USGS_GAGES <- gagelist$USGS_GAGES 

#j<-10
#j<-9
#j<-6

#Begin loop to run through each USGS gage 
for (j in 1:length(USGS_GAGES)) {
  USGS_GAGE_ID <- USGS_GAGES[j]
  print(paste("USGS_GAGE_ID ", USGS_GAGE_ID, sep='')); 

  
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
gage <- readNWISdv(USGS_GAGE_ID,'00060')
gage <- renameNWISColumns(gage)

#Calculate 7-Day Average Streamflow for every day of the historic record
rollmean_7day <- rollmeanr(gage$Flow,7,fill=NA)
gage <- cbind(gage, rollmean_7day )
#tail(gage)

#--Current ROLLING 7-DAY AVERAGE for USGS Streamgage
latest_row <- gage[length(gage$Date),]
rolling_7day_avg <- latest_row$rollmean_7day


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

  if ((rolling_7day_avg < as.numeric(as.character(month_quant[9]))) == 'FALSE') {   #IF RECORD LOW
    rolling_percentile <- 100
  } else if ((rolling_7day_avg > as.numeric(as.character(month_quant[1]))) == 'FALSE') {   #IF RECORD HIGH
    rolling_percentile <- 0
  } else if ((rolling_7day_avg < as.numeric(as.character(month_quant[1])) && rolling_7day_avg > as.numeric(as.character(month_quant[9]))) == 'FALSE') {  #BETWEEN HISTORIC LOW AND HISTORIC HIGH
    
  for (i in 1:length(quant_num)) {  

    lower_quantile <- quant_num[i]
    upper_quantile <- quant_num[i]+1
    lower_percent <- quant[i]
    upper_percent <- quant[i+1]
    
        if ((rolling_7day_avg >= month_quant[lower_quantile] && rolling_7day_avg <= month_quant[upper_quantile] ) == 'TRUE') {
         rolling_percentile <- lower_percent + (rolling_7day_avg- month_quant[lower_quantile])*(( upper_percent - lower_percent)/(month_quant[upper_quantile]-month_quant[lower_quantile]))
         rolling_percentile <- as.vector(rolling_percentile)
         rolling_percentile <- rolling_percentile*100
        }
        }
  }
print(paste("The Rolling 7-day Avg for today is ",rolling_7day_avg,sep=""))
print(paste("This is equivalent to a percentile of ",rolling_percentile," for the month of ",month," at GAGE ",USGS_GAGE_ID,sep=""))

#Determine the drought status based on percentile thresholds 
if ((rolling_percentile  > 25) == 'TRUE') {nonex_propcode <- 0}
if ((rolling_percentile  >= 10 && rolling_percentile <= 25) == 'TRUE') {nonex_propcode <- 1}
if ((rolling_percentile >= 5 && rolling_percentile  < 10) == 'TRUE') {nonex_propcode <- 2}
if ((rolling_percentile  < 5) == 'TRUE') {nonex_propcode <- 3}
print(paste("The Drought Status Propcode for ",USGS_GAGE_ID," is ",nonex_propcode,sep=""))

#--STORE WITH REST 'drought_status_stream', 'q_7day_cfs', and 'nonex_pct'

#Set values to dH variables
q_7day_cfs <- rolling_7day_avg
nonex_propcode <- nonex_propcode
nonex_propvaue <- rolling_percentile

#-----------------------------------------------
#If statement to handle negative flow values resulting from Ice buildup at gage sites (or any other equipment failures)

if (latest_row$rollmean_7day < 0.00) {   
  q_7day_cfs <- NULL
  nonex_propcode <- "No Data"
  nonex_propvaue <- NULL
} 
#-----------------------------------------------

#Convert USGS Site No. to dH Hydrocode 
siteNumber <- USGS_GAGE_ID
hydrocode <- paste("usgs_",siteNumber, sep="")

#Retrieve dH usgsgage feature 
gage_feature <- GET(paste(site,"/dh_feature.json",sep=""), 
                    add_headers(HTTP_X_CSRF_TOKEN = token),
                    query = list(bundle = 'usgsgage',
                                 hydrocode = hydrocode
                    ), 
                    encode = "json"
);
gage <- content(gage_feature);
gage <- gage$list[[1]];
hydroid = gage$hydroid[[1]]

#Convert start and endate to UNIX timestamp (to be used with timeseries)
startdate <- as.numeric(as.POSIXct(Sys.Date()-6,origin = "1970-01-01", tz = "GMT"))
enddate <- as.numeric(as.POSIXct(Sys.Date(),origin = "1970-01-01", tz = "GMT"))

#Set dH Variables 
propvars <- c(
  'q_7day_cfs',
  'drought_status_stream'
);

proplist <- list(
  q_7day_cfs = FALSE,
  drought_status_stream = FALSE
);

#i<-1
#Begin Loop to REST properties one at a time 
for (i in 1:length(propvars)) {
  
  propdef_url<- paste(site,"/?q=vardefs.tsv/all/drought",sep="");
  propdef_table <- read.table(propdef_url,header = TRUE, sep = "\t")    
  
  varkey <- propvars[i];
  print(varkey); 
  
  # retrieve varid
  varid <- propdef_table[1][which(propdef_table$varkey == varkey),];
  print(paste("Found varid ", varid));
  
  if ((varkey == 'q_7day_cfs')) {
    tsvalue = q_7day_cfs;
    tstime <- startdate 
    tsendtime <- enddate 
    
    #Format timeseries 
    pf <- list(
      varid = varid,
      tsvalue = tsvalue,
      tstime = tstime,
      tsendtime = tsendtime,
      tscode = '',
      featureid = hydroid,
      entity_type = 'dh_feature'
    );  
    
    
    #-----RETRIEVE TIMESERIES   
    timeseries_gwl <- GET(paste(site,"/dh_timeseries.json",sep=""), 
                          add_headers(HTTP_X_CSRF_TOKEN = token),
                          query = list(
                            featureid = pf$featureid,
                            varid = varid,
                            tstime = pf$tstime,
                            tsendtime = pf$tsendtime,
                            entity_type = 'dh_feature'
                          ), 
                          encode = "json"
    );
    timeseries_gwl <- content(timeseries_gwl);
    
    #------CREATE TIMESERIES IF ONE DOES NOT EXIST     
    pbody = list(
      featureid = pf$featureid,
      varid = pf$varid,
      entity_type = 'dh_feature',
      tsvalue = pf$tsvalue,
      tstime = pf$tstime,
      tsendtime = pf$tsendtime,
      tscode = NULL
    );
    
    #Update TIMESERIES if it exists    
    if (length(timeseries_gwl$list)) {
      timeseries_gwl <- timeseries_gwl$list[[1]];
      tid <-  timeseries_gwl$list[[1]]$tid
      print(paste("tid: ", tid, "tsvalue", pbody$tsvalue));
      #** PUT - Update
      print ("Timeseries exists - PUT q_7day_cfs");
      sub <- PUT(paste(site,"/dh_timeseries/",tid,sep=''), 
                 add_headers(HTTP_X_CSRF_TOKEN = token),
                 body = pbody, 
                 encode = "json"
      );
      
      #content(sub)
      
      #Create Timeseries if it does not exist        
    } else {
      print ("Timeseries does not exist - POST q_7day_cfs");
      #** POST - Insert
      x <- POST(paste(site,"/dh_timeseries/",sep=""), 
                add_headers(HTTP_X_CSRF_TOKEN = token),
                body = pbody,
                encode = "json"
      );
    }
    
    
    #------ATTATCH PROPERTY 'nonex_pct' TO TIMESERIES   
    
    #-----RETRIEVE TIMESERIES   
    timeseries_gwl <- GET(paste(site,"/dh_timeseries.json",sep=""), 
                          add_headers(HTTP_X_CSRF_TOKEN = token),
                          query = list(
                            featureid = pf$featureid,
                            varid = varid,
                            tstime = pf$tstime,
                            tsendtime = pf$tsendtime,
                            entity_type = 'dh_feature'
                          ), 
                          encode = "json"
    );
    timeseries_gwl <- content(timeseries_gwl)
    tid <- timeseries_gwl$list[[1]]$tid
    
    
    #---------------------------------------------------------
    # retrieve varid
    nonex_pct_varid <- propdef_table[1][which(propdef_table$varkey == 'nonex_pct'),];
    print(paste("Found nonex_pct varid ", nonex_pct_varid));
    
    #Format property  
    pf <- list(
      varid =  nonex_pct_varid,
      propname = 'nonex_pct',
      propvalue = nonex_propvaue,
      propcode = nonex_propcode,
      tid = tid,
      bundle = 'dh_properties',
      entity_type = 'dh_timeseries'
    );  
    
    #Retrieve property if it exists   
    sp <- GET(
      paste(site,"/dh_properties.json",sep=""), 
      add_headers(HTTP_X_CSRF_TOKEN = token),
      query = list(
        bundle = 'dh_properties',
        #tid = pf$tid,
        featureid = pf$tid,
        varid = pf$varid,
        entity_type = 'dh_timeseries'
        
      ), 
      encode = "json"
    );
    spc <- content(sp);  
    
    pbody = list(
      bundle = 'dh_properties',
      # tid = pf$tid,
      featureid = pf$tid,
      varid = pf$varid,
      entity_type = 'dh_timeseries',
      propname = pf$propname,
      propvalue = pf$propvalue,
      propcode = pf$propcode
    );
    
    #Update property if it exists    
    if (length(spc$list)) {
      spe <- spc$list[[1]];
      print ("Property exists - PUT nonex_pct");
      pid <- spe$pid[[1]];
      print(paste("pid: ", pid, "propcode", pbody$propcode));
      #** PUT - Update
      sub <- PUT(paste(site,"/dh_properties/",pid,sep=''), 
                 add_headers(HTTP_X_CSRF_TOKEN = token),
                 body = pbody, 
                 encode = "json"
      );
      #Create property if it does not exist        
    } else {
      print ("Property does not exist - POST nonex_pct");
      #** POST - Insert
      x <- POST(paste(site,"/dh_properties/",sep=""), 
                add_headers(HTTP_X_CSRF_TOKEN = token),
                body = pbody,
                encode = "json"
      );
    } 
    
    
    #------SET drought_status_stream PROPERTY    
    #----------------------------------------------------------------------------   
  } else { ((varkey == 'drought_status_stream')) 
    propval = nonex_propvaue;
    
    
    #Format property  
    pf <- list(
      varid = varid,
      propname = varkey,
      propvalue = propval,
      propcode = '',
      featureid = hydroid,
      bundle = 'dh_properties',
      entity_type = 'dh_feature'
    );  
    
    #Retrieve property if it exists   
    sp <- GET(
      paste(site,"/dh_properties.json",sep=""), 
      add_headers(HTTP_X_CSRF_TOKEN = token),
      query = list(
        bundle = 'dh_properties',
        featureid = pf$featureid,
        varid = varid,
        entity_type = 'dh_feature'
        
      ), 
      encode = "json"
    );
    spc <- content(sp);  
    
    pbody = list(
      bundle = 'dh_properties',
      featureid = pf$featureid,
      varid = pf$varid,
      entity_type = 'dh_feature',
      propname = pf$propname,
      propvalue = pf$propvalue,
      propcode = NULL
    );
    
    if ((varkey == 'q_7day_cfs')) {
      pbody$propcode = NULL;
      pbody$propvalue = q_7day_cfs;
    } else { ((varkey == 'drought_status_stream')) 
      pbody$propcode = nonex_propcode;
      pbody$propvalue = nonex_propvaue;
    }
    
    
    #Update property if it exists    
    if (length(spc$list)) {
      spe <- spc$list[[1]];
      print ("Property exists - PUT drought_status_stream");
      pid <- spe$pid[[1]];
      print(paste("pid: ", pid, "propcode", pbody$propcode));
      #** PUT - Update
      sub <- PUT(paste(site,"/dh_properties/",pid,sep=''), 
                 add_headers(HTTP_X_CSRF_TOKEN = token),
                 body = pbody, 
                 encode = "json"
      );
      #Create property if it does not exist        
    } else {
      print ("Property does not exist - POST drought_status_stream");
      #** POST - Insert
      x <- POST(paste(site,"/dh_properties/",sep=""), 
                add_headers(HTTP_X_CSRF_TOKEN = token),
                body = pbody,
                encode = "json"
      );
    }
    
  } #end of drought_status_stream property loop  
  
} #end of REST LOOP
} #end of gage feature loop

