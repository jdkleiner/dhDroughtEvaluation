## Calculating MLLR values and saving as file -- dh compatible ##
# Should update once a year on March 1st (tstime is March 1st for last year of data)

# Load necessary libraries
library('zoo')
library('IHA')
library('stringr')
library('lubridate')

calyear <- 1931;

file_directory <- "/var/www/html/files/dh/mllr/out/";
#file_directory <- "C:\\WorkSpace\\tmp\\";
file_name <- paste("mllr_drought_probs_", calyear, ".tsv", sep="");
file_path <- paste(file_directory, file_name, sep="");


#rm(list = ls())   # clear variables
# Initialize variables		
month <- c("july", "august", "september")
percentile <- c("05", "10", "25", "50")
e <- 1
c <- 1 
variables <- c()

m <- 0
errors <- c()
urls <- c()
gage_probs = data.frame(
  "hydrocode" = character(), 
  "tsvalue" = numeric(), 
  "varkey" = character(), 
  "calc_date" = integer(), 
  "tstime" = integer(), 
  "hcode" = character(), 
  stringsAsFactors = FALSE
);

#colnames(wshed_summary_tbl) <- c("Segment Name (D. Area)", "7Q10/ALF/Min Month", "WD (mean/max)" );

# Create the variable names
for (j in 1:length(month)) {
	for (k in 1:length(percentile)) {
		variables[e] <- paste("mllr", month[j], percentile[k], sep="_")
		e <- e + 1
	}
}

# retrieve all with un-set sept 10 probabilities for the current year
uri <- "http://deq1.bse.vt.edu/d.dh/usgs-mllr-sept10-gages-all"
gagelist = read.csv(uri, header = TRUE, sep = "\t");
gage <- gagelist$staid
# un-comment to test a small set
#gage <- c(01636316)
#gage <- as.numeric(gage)
gage <- sprintf("%08d", gage)

## MLLR calculation ##

for (i in 1:length(gage)) {   
	# reset variables	
	z <- 1 
	P_est <- c()
	winterflow <- c()	

	# Extract flow data for the current year from Nov - Feb
	StartDate <- as.Date(paste((calyear-1), "-11-01", sep=""))
	EndDate <- as.Date(paste(calyear, "-02-28", sep=""))
	hydrocode <- paste("usgs", gage[i], sep="_")
	
	# Extracting winter flow data
	url_base <- "https://waterservices.usgs.gov/nwis/dv/?variable=00060&format=rdb&startDT="
	url <- paste(url_base, StartDate, "&endDT=", EndDate, "&site=", gage[i], sep="")	
  print(paste("Trying ", url, sep=''));
	winterflow <- try(read.table(url, skip = 2, comment.char = "#"))

	# No 2014 estimates if there is no 2014 winter flow data 
	if (class(winterflow)=="try-error") { 	
		next
	} 
			
	# No 2014 estimates for gages without a column for flow data on USGS
	if (ncol(winterflow) < 4) {
		next
	}
	
	# Determine the average November through February mean daily flow                  
	n_f_flow <- mean(na.omit(as.numeric(as.vector(winterflow[,4]))))
	### Getting Beta Values ONCE ###
	b<-0
	beta_0 <- c()
	beta_1 <- c()
	beta_table <- c()
	for (J in 1:length(month)) {
		for (K in 1:length(percentile)) {
			# Write beta variable names
			b <- b+1
			beta_0[b] <- paste("mllr_beta0", month[J], percentile[K], sep="_")
			beta_1[b] <- paste("mllr_beta1", month[J], percentile[K], sep="_")
		} 
	} 
	# Combine beta0s and beta1s 
	varkeys <- c(beta_0, beta_1)
	# Write get_modelData URL to get ALL 24 beta values
	beta_url <- paste("http://deq1.bse.vt.edu/d.dh/?q=export-properties-om/usgs_", gage[i], "/drought_model", sep="");
  print(paste("getting betas: ", beta_url, sep=""));
	# Read beta values and names from URL
	rawtable <- try(read.table(beta_url, header=TRUE, sep=","))
	if (class(rawtable)=="try-error") { 	
		next
	} else { 
	  beta_table <- cbind(as.character(rawtable$dataname), rawtable$dataval) 
	}

	### End pulling in beta values ###
	for (j in 1:length(month)) {
				
		for (k in 1:length(percentile)) {

			## Getting the correct beta values for the month and percentile ##
			varkey_beta_0 <- paste("mllr_beta0", month[j], percentile[k], sep="_")
			varkey_beta_1 <- paste("mllr_beta1", month[j], percentile[k], sep="_")
      print(paste("Searching for varkeys ", varkey_beta_0, varkey_beta_1, sep=" "));

			b0 <- as.numeric(beta_table[beta_table[,1]==varkey_beta_0,2])
			b1 <- as.numeric(beta_table[beta_table[,1]==varkey_beta_1,2])
      print(paste("Found ", b0, b1, sep=""));
      
      if (length(b0) & length(b1)) {
        ## Calculating P_est in the given month for the given percentile ##

        P_est <- 1/(1+exp(-(b0 + b1*n_f_flow)));
        
        # Creating columns for file output
        c <- c + 1 # total count of how many probabilities are being calculated
        newline = data.frame( 
          "hydrocode" = hydrocode, 
          "tsvalue" = P_est, 
          "varkey" = variables[z], 
          "tstime" =  as.numeric(as.POSIXct(paste(paste(calyear, "-03-01", sep=""),"EST"))), 
          "hcode" = paste("usgs", gage[i], sep="_")
        );
        gage_probs <- rbind(gage_probs, newline);
      } else {
        print(paste(variables[z], " = NULL for gage", gage[i], " varkeys ", varkey_beta_0, varkey_beta_1, sep=" "));
      }
      z <- z + 1 # count for which mllr value this gage is on
    } # Ends percentile loop		
  } # Ends month loop
	
  print(paste("Finished gage", gage[i], sep=" "))

} # Ends gage loop

# See results	
print(gage_probs)

write(c(), file = file_path)  #create file where image will be saved
write.table(gage_probs, file = file_path,append=FALSE,quote=FALSE,row.names=FALSE,col.names=c('hydrocode','tsvalue','varkey','tstime','hcode'),sep="\t")
