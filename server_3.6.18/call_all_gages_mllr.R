## Calculating MLLR values and saving as file -- dh compatible ##
# Should update once a year on March 1st (tstime is March 1st for last year of data)

# Load necessary libraries
library('zoo')
library('IHA')
library('stringr')
library('lubridate')

file_directory <- "/var/www/html/files/dh/mllr/out/";
file_name <- paste("mllr_drought_probs_", year(Sys.Date()), ".tsv", sep="");
file_path <- paste(file_directory, file_name, sep="");


#rm(list = ls())   # clear variables

# Initialize variables		
month <- c("july", "august", "september")
percentile <- c("05", "10", "25", "50")
hydrocode <- c()
tsvalue <- c()
varkey <- c()
tstime <- c()
hcode <- c()
variables <- c()
e <- 1
c <- 1 

m <- 0
errors <- c()
urls <- c()


# Create the variable names
for (j in 1:length(month)) {
	for (k in 1:length(percentile)) {
		variables[e] <- paste("mllr", month[j], percentile[k], sep="_")
		e <- e + 1
	}
}

# Set the gage numbers as a vector
gage <- c(1632082);
# or retrieve all with un-set sept 10 probabilities for the current year
uri <- "http://deq1.bse.vt.edu/d.dh/usgs-mllr-sept10-gages"
gagelist = read.csv(uri, header = TRUE, sep = "\t");
gage <- gagelist$staid

gage <- as.numeric(gage)
gage <- sprintf("%08d", gage)
# un-comment to test a small set
#gage <- c("01654000", "01634000")

## MLLR calculation ##

for (i in 1:length(gage)) {   
	
	# reset variables	
	z <- 1 
	P_est <- c()
	winterflow <- c()	

	# Extract flow data for the current year from Nov - Feb
	StartDate <- as.Date(paste((year(Sys.Date())-1), "-11-01", sep=""))
	EndDate <- as.Date(paste(year(Sys.Date()), "-02-28", sep=""))

	# Extracting winter flow data
	url_base <- "https://waterservices.usgs.gov/nwis/dv/?variable=00060&format=rdb&startDT="
	url <- paste(url_base, StartDate, "&endDT=", EndDate, "&site=", gage[i], sep="")	
	winterflow <- try(read.table(url, skip = 2, comment.char = "#"))

		# No 2014 estimates if there is no 2014 winter flow data 
		if (class(winterflow)=="try-error") { 	
			# create columns for output if betas do not exist
			hydrocode[c:(c+11)] <- rep(paste("usgs", gage[i], sep="_"), 12)
			tsvalue[c:(c+11)] <- rep("NA", 12)
			varkey[c:(c+11)] <-  variables
			calc_date <- paste(year(Sys.Date()), "-03-01", sep="")
			tstime[c] <- as.numeric(as.POSIXct(paste(calc_date,"EST")))
			hcode[c:(c+11)] <- rep(paste("usgs", gage[i], sep="_"), 12)
			c <- c + 1		
			next
		} 
				
		# No 2014 estimates for gages without a column for flow data on USGS
		if (ncol(winterflow) < 4) {
			# create columns for output if betas do not exist
			hydrocode[c:(c+11)] <- rep(paste("usgs", gage[i], sep="_"), 12)
			tsvalue[c:(c+11)] <- rep("NA", 12)
			varkey[c:(c+11)] <-  variables
			calc_date <- paste(year(Sys.Date()), "-03-01", sep="")
			tstime[c] <- as.numeric(as.POSIXct(paste(calc_date,"EST")))
			hcode[c:(c+11)] <- rep(paste("usgs", gage[i], sep="_"), 12)
			c <- c + 1
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
		
		# create columns for output if betas do not exist
		hydrocode[c:(c+11)] <- rep(paste("usgs", gage[i], sep="_"), 12)
		tsvalue[c:(c+11)] <- rep("NA", 12)
		varkey[c:(c+11)] <-  variables
		calc_date <- paste(year(Sys.Date()), "-03-01", sep="")
		tstime[c:(c+11)] <- rep(as.numeric(as.POSIXct(paste(calc_date,"EST"))), 12)
		hcode[c:(c+11)] <- rep(paste("usgs", gage[i], sep="_"), 12)
		c <- c + 12
		next

	} else { beta_table <- cbind(as.character(rawtable$dataname), rawtable$dataval) }


	### End pulling in beta values ###

	
	for (j in 1:length(month)) {
				
		for (k in 1:length(percentile)) {

			## Getting the correct beta values for the month and percentile ##
			varkey_beta_0 <- paste("mllr_beta0", month[j], percentile[k], sep="_")
			varkey_beta_1 <- paste("mllr_beta1", month[j], percentile[k], sep="_")
      print(paste("Searching for varkeys ", varkey_beta_0, varkey_beta_1, sep=" "));

			b0 <- as.numeric(beta_table[beta_table[,1]==varkey_beta_0,2])
			b1 <- as.numeric(beta_table[beta_table[,1]==varkey_beta_1,2])
      print(paste("Found ", b0, b1, sep=","));
      
      if (length(b0) & length(b1)) {
        ## Calculating P_est in the given month for the given percentile ##

        P_est <- 1/(1+exp(-(b0 + b1*n_f_flow)));
        
        # Creating columns for file output
        hydrocode[c] <- paste("usgs", gage[i], sep="_")
        tsvalue[c] <- P_est
        varkey[c] <- variables[z]
        calc_date <- paste(year(Sys.Date()), "-03-01", sep="")
        tstime[c] <- as.numeric(as.POSIXct(paste(calc_date,"EST")))
        hcode[c] <- paste("usgs", gage[i], sep="_")
    
        z <- z + 1 # count for which mllr value this gage is on
        c <- c + 1 # total count of how many probabilities are being calculated
			} else {
        print(paste("NULL values for gage", gage[i], " varkeys ", varkey_beta_0, varkey_beta_1, sep=" "));
      }
		} # Ends percentile loop
		
	} # Ends month loop
	
	print(paste("Finished gage", gage[i], sep=" "))

} # Ends gage loop


	# See results	
	results <- cbind(hydrocode, tsvalue, varkey, tstime, hcode)
	print(results)

write(c(), file = file_path)  #create file where image will be saved
write.table(results, file = file_path,append=FALSE,quote=FALSE,row.names=FALSE,col.names=c('hydrocode','tsvalue','varkey','tstime','hcode'),sep="\t")
