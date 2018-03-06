#Load required libraries 
library(dataRetrieval) #https://cran.r-project.org/web/packages/dataRetrieval/dataRetrieval.pdf

streamgage_historic <- function(gageid){
  print(paste("Retrieving Historic Data for: ",gageid, sep='')); 

  #Retrieve Extra Gage Attribute Info - Such as gage station name
  ##gage_info <- readNWISsite(gageid)
  ##staname <- gage_info$station_nm
  ##print(paste("Station Name: ",staname, sep='')); 

  #Retrieve all historic gage streamflow data in cfs
  gage_data <- readNWISdv(gageid,'00060')
  gage_data <- renameNWISColumns(gage_data)

  # Skip gages that don't have a column for flow data from USGS
  if (ncol(gage_data) < 5) {
    print(paste("NWIS Responded NO DATA for ", gageid, sep=''))
    next
  } else {
    gage_data <- gage_data 
  }
  
} #Close Function


clean_historic <- function(historic){
    # ******************************************************************************************
    # Remove any rows with "Ice", "P Ice" or "P Eqp" values for Flow_cd
    # ******************************************************************************************
    if (length(which(historic[,5]== "Ice")) != 0 ){
      print("Removing Ice Flows")
      historic <- historic[-which(historic[,5]== "Ice"),]
    }
      
    if (length(which(historic[,5]== "P Ice")) != 0 ){
      print("Removing Ice Flows")
      historic <- historic[-which(historic[,5]== "P Ice"),]
    }
      
    if (length(which(historic[,5]== "P Eqp")) != 0 ){
      print("Removing P Eqp Flows")
      historic <- historic[-which(historic[,5]== "P Eqp"),]
    }
      
    if (length(which(historic[,4] < 0.0)) != 0 ){
      print("Removing Negative Flow Values")
      historic <- historic[-which(historic[,4] < 0.0),]
    }
  
    historic <- historic
} #Close Function
    
    
    
# Additional groundwater level function to come....
#groundwater_historic