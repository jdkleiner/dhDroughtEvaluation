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

#Specify the site of interest
#site <- "http://localhost/d.dh"    
site <- "http://deq1.bse.vt.edu/d.dh" 
fxn_locations <- "C:\\Users\\nrf46657\\Desktop\\VAHydro Development\\Drought\\server_code\\"          #Specify location of supporting function .R files

#retrieve rest token
source(paste(fxn_locations,"dh_rest_token.R", sep = ""));     #loads function used to generate rest session token
dh_rest_token (site, token)
token <- dh_rest_token(site, token)

#Pull in list of all drought USGS well dH Features 
URL <- paste(site,"drought-wells-export", sep = "/");
well_list <- read.table(URL,header = TRUE, sep = ",")
hydrocodes <- well_list$hydrocode

#j <-1

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
data$min <- as.numeric(as.character(data[,7])) #COPY MIN (Depth to water level) COLUMN AND FORCE 'NA's WHERE THERE IS MISSING DATA
data$mean <- rowMeans(subset(data, select = c(max, min)), na.rm = TRUE) #CREATE COLUMN OF MEAN OF MAX AND MIN COLUMNS

#most recent gwl reading (mean of daily max and min depth to water levels)
latest_row <- length(data$mean)
latest_row <- data[latest_row,]
gw_lvl <- latest_row$mean
gw_lvl <- as.numeric(as.character(gw_lvl)) 
#need to handle 'EDT' for most recent value (use the day before)
if  (is.na(gw_lvl) == TRUE) {
  latest_row <- length(data$mean)
  latest_row <- latest_row - 1
  latest_row <- data[latest_row,]
  gw_lvl <- latest_row$mean
  gw_lvl <- as.numeric(as.character(gw_lvl))
}
print(gw_lvl) #print most recent daily mean gwl reading 

rollmean_7day <- rollapply(data$mean, 7, mean, na.rm=TRUE)  #BUILD VECTOR OF 7-DAY ROLLING MEAN OF DAILY MEAN GWL
  rollmean_7day  <- append(rollmean_7day , NA, after = 0)   #ROLLING MEAN FUNCTION EXCLUDED FIRST 6 DATA VALUES
  rollmean_7day  <- append(rollmean_7day , NA, after = 0)     #MUST MANUALLY SET 'NA's FOR THE FIRST 6 VALUES
  rollmean_7day  <- append(rollmean_7day , NA, after = 0)
  rollmean_7day  <- append(rollmean_7day , NA, after = 0)
  rollmean_7day  <- append(rollmean_7day , NA, after = 0)
  rollmean_7day  <- append(rollmean_7day , NA, after = 0)
data$rollmean_7day <- rollmean_7day                         #CREATE COLUMN OF 7-DAY ROLLING MEANS ON DATAFRAME

latest_row <- data[length(data$rollmean_7day),] #most recent 7-day rolling avg gwl
gw_lvl <- latest_row$rollmean_7day
print(gw_lvl) #print most recent 7-day rolling average daily mean gwl reading 

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

#Determine The percentiles for this month based on 7-Day Average Streamflow
data [ grep( month_num , data$datetime, perl= TRUE ), "month" ] <- month
month_rows <- which(data$month == month)
month_data <- data[month_rows,]
#write.csv(month_data, "month_data.csv")

#remove rows with emptys
month_data <- month_data[!(is.na(month_data$rollmean_7day) | month_data$rollmean_7day==""), ]
month_gwls <- month_data$rollmean_7day
month_gwls <- as.numeric(as.character(month_gwls))
#tail(month_gwls)

quant_num <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
quant <- c(0, 0.05, 0.10, 0.25, 0.5, 0.75, 0.90, 0.95, 1)
month_quant <- quantile(month_gwls, probs =  quant) 

#gw_lvl <-9
#i<-1

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


#--STORE WITH REST 'drought_status_well', 'gwl_7day_ft', and 'nonex_pct'

#Set values to dH variables
gwl_7day_ft <- gw_lvl
nonex_propcode <- nonex_propcode
nonex_propvaue <- rolling_percentile

nonex_propcode <- as.numeric(as.character(nonex_propcode))
nonex_propvaue <- as.numeric(as.character(nonex_propvaue))


#Convert USGS well No. to dH Hydrocode 
hydrocode <- paste("usgs_",siteNumber, sep="")

#Retrieve dH usgsgage feature 
well_feature <- GET(paste(site,"/dh_feature.json",sep=""), 
                    add_headers(HTTP_X_CSRF_TOKEN = token),
                    query = list(bundle = 'well',
                                 hydrocode = hydrocode
                    ), 
                    encode = "json"
);
well <- content(well_feature);
well <- well$list[[1]];
hydroid = well$hydroid[[1]]

#Convert start and endate to UNIX timestamp (to be used with timeseries)
startdate <- as.numeric(as.POSIXct(Sys.Date()-6,origin = "1970-01-01", tz = "GMT"))
enddate <- as.numeric(as.POSIXct(Sys.Date(),origin = "1970-01-01", tz = "GMT"))

#Set dH Variables 
propvars <- c(
  'gwl_7day_ft',
  'drought_status_well'
);

proplist <- list(
  gwl_7day_ft = FALSE,
  drought_status_well = FALSE
);

#i <- 1

#Begin Loop to REST properties one at a time 
for (i in 1:length(propvars)) {
  
  propdef_url<- paste(site,"/?q=vardefs.tsv/all/drought",sep="");
  propdef_table <- read.table(propdef_url,header = TRUE, sep = "\t")    
  
  varkey <- propvars[i];
  print(varkey); 
  
  # retrieve varid
  varid <- propdef_table[1][which(propdef_table$varkey == varkey),];
  print(paste("Found varid ", varid));
  
  if ((varkey == 'gwl_7day_ft')) {
    tsvalue = gwl_7day_ft;
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
      print ("Timeseries exists - PUT gwl_7day_ft");
      sub <- PUT(paste(site,"/dh_timeseries/",tid,sep=''), 
                 add_headers(HTTP_X_CSRF_TOKEN = token),
                 body = pbody, 
                 encode = "json"
      );
      #Create Timeseries if it does not exist        
    } else {
      print ("Timeseries does not exist - POST gwl_7day_ft");
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
    

#------SET drought_status_well PROPERTY    
 #----------------------------------------------------------------------------   
  } else { ((varkey == 'drought_status_well')) 
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
  
  if ((varkey == 'gwl_7day_ft')) {
    pbody$propcode = NULL;
    pbody$propvalue = gwl_7day_ft;
  } else { ((varkey == 'drought_status_well')) 
    pbody$propcode = nonex_propcode;
    pbody$propvalue = nonex_propvaue;
  }
  
  
  #Update property if it exists    
  if (length(spc$list)) {
    spe <- spc$list[[1]];
    print ("Property exists - PUT drought_status_well");
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
    print ("Property does not exist - POST drought_status_well");
    #** POST - Insert
    x <- POST(paste(site,"/dh_properties/",sep=""), 
              add_headers(HTTP_X_CSRF_TOKEN = token),
              body = pbody,
              encode = "json"
    );
  }
    
  } #end of drought_status_well property loop  
  
} #end of REST LOOP
} #end of well feature loop

