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
  
}

# Additional groundwater level function to come....
#groundwater_historic