## Code to calculate the current drought condition estimates and create a graph to show all estimates by month and threshold 
# This code will be updated using "chron" every March 1st

#lib_directory <- "/var/www/R/r-dh-ecohydro/Analysis/fn_vahydro-2.0"  
#auth_directory <- "/var/www/R/r-dh-ecohydro/ELFGEN/internal"  
#file_directory <- "/var/www/html/images/dh"  
base_url <- 'http://deq1.bse.vt.edu/d.dh' #needed for REST functions

lib_directory <- "C:\\Users\\nrf46657\\Desktop\\mllr_2.5.18\\gitlab\\"
auth_directory <- lib_directory
file_directory <- paste(lib_directory,"\\plots",sep="")


uri <- "http://deq1.bse.vt.edu/d.dh/usgs-mllr-sept10-gages-all"
gagelist = read.csv(uri, header = TRUE, sep = "\t");
gage <- paste0("0", gagelist$staid)

#fid needed for retrieving beta properties via REST
fid <- 58660 #58728
target_year=2018
#gage <- c(02042500) 

gage <- sprintf("%08d", gage)
gage <- as.character(gage)

# Load necessary libraries
library('zoo')
library('IHA')
library('stringr')
library('lubridate')
library('ggplot2')
library('scales')
library('httr')

source(paste(lib_directory,"rest_functions.R", sep = "/")); 
source(paste(lib_directory,"usgs_gage_functions.R", sep = "")); #contains function for retrieving historic gage data
source(paste(auth_directory,"rest.private",sep="/")); 
token <- rest_token (base_url, token, rest_uname = rest_uname, rest_pw = rest_pw) #token needed for REST

#i <- 1
for (i in 1:length(gage)) {

	# Initialize variables
	n_f_flow <- c()
	year <- year(Sys.Date())
	most_recent = -99999;
        # @todo: this looks like it is no longer used
	# Find the element id for the USGS gage
	#elem_url <- paste("http://deq1.bse.vt.edu/om/remote/get_modelData.php?variables=elementid,elemname&scenarioid=17&querytype=custom2&operation=7&custom1=usgs_stream_gage&params=", gage[i], sep="")
	#elem_info <- scan(elem_url, what="character", sep=",")
	#elid <- elem_info[3]

	# Retrieve all historic flow data
	historic <- streamgage_historic(gage[i])

	
	# ******************************************************************************************
	# Remove any rows with "Ice", "P Ice" or "P Eqp" values for Flow_cd
	# ******************************************************************************************
		if (length(which(historic[,5]== "Ice")) != 0 ){
		    print("Removing Ice Flows")
        historic <- historic[-which(historic[,5]== "Ice"),]
		} else if (length(which(historic[,5]== "P Ice")) != 0 ){
		    print("Removing Ice Flows")
		    historic <- historic[-which(historic[,5]== "P Ice"),]
		} else if (length(which(historic[,5]== "P Eqp")) != 0 ){
		  print("Removing P Eqp Flows")
		  historic <- historic[-which(historic[,5]== "P Eqp"),]
		} else {
		    print("No Ice Flows in Historic Record")
		    historic <- historic
	  }
	# ******************************************************************************************
	# ******************************************************************************************
	
	# Find first and last date on record
	start.date <- as.Date(as.character(historic[1,3]))
	end.date <- as.Date(as.character(tail(historic[,3],1))) 
		
	# Makes the StartDate the earliest year with November on record
	if (start.date > as.Date(paste(year(start.date), '-11-01', sep=""))) {
		year <- year(start.date) + 1
	} else { year <- year(start.date) }	
	
	
	# Iterate for all of the years in the gage's dataset
	for (z in 1:(year(end.date)-year)) {		
	
		StartDate <- as.Date(paste(year, "-11-01", sep="")) 
		EndDate <- as.Date(paste((year+1), "-02-28", sep=""))
	
		# Extract flow data for the specified year from Nov - Feb (gathering all historic winter flows)	
		f <- historic[,4][(as.Date(historic$Date)>=StartDate) & (as.Date(historic$Date)<=EndDate)]
			
		# Determine the average November through February mean daily flow                  
		n_f_flow[z] <- mean(na.omit(as.numeric(as.vector(f))))
		if ((year + 1) == target_year) {
		  most_recent <- round(tail(na.omit(n_f_flow), n=1), digits=0) # the target year's average winter flow
		}
		year <- year + 1
	}
	
	median <- round(median(na.omit(n_f_flow)), digits=0) # the median winter flow for the entire record	
	
	# set up data frame for the most recent and median winter flow values
	names <- c(paste(target_year," Winter Flow",sep=""), "Median Winter Flow")
	if (most_recent != -99999) {
	  values <- c(most_recent, median)
	} else {
	  values <- c(median, median)
	}
	lines <- data.frame(names, values)
	
	plot_xtitle <- "Winter Flow, cfs"
	plot_xsubtitle <- "Average of daily mean flows from November to February of each year on record"
	
	# Saving file to the correct location
	filename <- paste("usgs", gage[i], "mllr_bar_winterflows", target_year, ".png", sep="_")
	
	# ******************************************************************************************
	# Plot 1: Create histogram/density plot to show recent winter flow vs historic winter flows
	# ******************************************************************************************
	plt <- ggplot() + 
	  
	  # plot the histogram
	  geom_histogram(aes(x=n_f_flow), colour="black", fill = "lightblue",bins = 30) + 
	  
	  # plot the lines at the most recent and median winter flows
	  geom_vline(data=lines, aes(xintercept=values, color=names), linetype="longdash", size=1, show_guide=TRUE) +
	  
	  # Y scale showing only integers
	  scale_y_continuous(breaks= pretty_breaks()) +
	  
	  # customize the legend
	  scale_color_manual(
	    values=c("#CC6666", "#666666"), 
	    labels=c(paste(target_year, " Winter Flow =", most_recent, "cfs"), paste("Median Winter Flow =", median, "cfs")),
	    name="") +
	  theme(legend.position="bottom") +	
	  guides(color=guide_legend(ncol=2)) +	
	  
	  # specify labels and title for the graph 
	  xlab(bquote(atop(.(plot_xtitle), italic(.(plot_xsubtitle), "")))) + ylab("Number of Winters") + 
	  ggtitle(paste("Distribution of Historic Winter Flows for", gage[i]))
	# ******************************************************************************************
	# ******************************************************************************************
	# End Plot 1
	#--------------------------------------------------------------------------
	# ******************************************************************************************
	# Table 1: Retreive the table of MLLR coefficients
	# ******************************************************************************************
	#Retrieve b0 and b1 properties 
	month <- c('july','august','september')
	
	
	beta_table <- data.frame('metric'=character(),
	                         'b0'=character(),
	                         'b1'=character(),
	                         stringsAsFactors=FALSE)
	
	#m <- 1
	for (m in 1:length(month)) {
	  
	  #retrieve b0 property
	  b0_inputs <- list(featureid = fid,varkey = paste('mllr_beta0_',month[m],'_10',sep=''),entity_type = 'dh_feature')
	  b0 <- getProperty (b0_inputs, base_url, prop)
	  b0 <- as.numeric(as.character(b0$propvalue))
	  
	  #retrieve b1 property
	  b1_inputs <- list(featureid = fid,varkey = paste('mllr_beta1_',month[m],'_10',sep=''),entity_type = 'dh_feature')
	  b1 <- getProperty (b1_inputs, base_url, prop)
	  b1 <- as.numeric(as.character(b1$propvalue))
	  
	  print(paste("b0: ",b0,sep=""))
	  print(paste("b1: ",b1,sep=""))
	  
	  beta_table_i <- data.frame(month[m],b0,b1)
	  beta_table <- rbind(beta_table, beta_table_i)
	  
	}
	#--------------------------------------------------------------------------
	# ******************************************************************************************
	# END Table 1
	# ******************************************************************************************
	# Output the beta_table 
	print(beta_table)
	
	# ******************************************************************************************
	# Plot 2: Plot the equation of MLLR on graph to illustrate intersection of flow & prob
	# ******************************************************************************************
  #Add MLLR Curves to Plot
	P_est <- function(b0, b1, x) {1/(1+exp(-(b0 + b1*x)))}

	#remove any rows with "Ice" values for locating maximum historic flow value 
#	if (length(which(historic[,4]== "Ice")) != 0 ){
	
#	  historic_no_ice <- historic[-which(historic[,4]== "Ice"),]
#	} else {
#	  historic_no_ice <- historic
#  }

	#need to determine x-range to use for plotting mllr curves, currently set to go from zero to 95th percentile flow
	#will cause number of histogram bins to be different than they were before
	xmax <- as.numeric(as.character(quantile(as.numeric(as.character(historic[,4])),0.95)))
  x <- c(0:xmax)

  july_est <- (P_est(beta_table[which(beta_table$month.m == "july"),"b0"], beta_table[which(beta_table$month.m == "july"),"b1"], x))*10
  july_mllr <- data.frame(x,(july_est))
  
  august_est <- (P_est(beta_table[which(beta_table$month.m == "august"),"b0"], beta_table[which(beta_table$month.m == "august"),"b1"], x))*10
  august_mllr <- data.frame(x,(august_est))
  
  september_est <- (P_est(beta_table[which(beta_table$month.m == "september"),"b0"], beta_table[which(beta_table$month.m == "september"),"b1"], x))*10
  september_mllr <- data.frame(x,(september_est))
  
  #add each month's P_est to plot 
  plt<-plt+geom_line(data=july_mllr,aes(x=x,y=july_est,color="red"),show.legend = FALSE)
  plt<-plt+geom_line(data=august_mllr,aes(x=x,y=august_est,color="red1"),show.legend = FALSE)
  plt<-plt+geom_line(data=september_mllr,aes(x=x,y=september_est,color="red2"),show.legend = FALSE)+
    
  scale_color_manual(
    values=c("#CC6666", "#666666","seagreen4","blue","orangered3"), 
    labels=c(paste(target_year, " Winter Flow =", most_recent, "cfs"), paste("Median Winter Flow =", median, "cfs"),"July 10th %ile","August 10th %ile","September 10th %ile"),
    name="")
  
  #add secondary y-axis to plot
  plt<-plt+scale_y_continuous(sec.axis = sec_axis(~.*10, name = "Probability Estimate"),breaks= pretty_breaks(),limits = c(0, 10))
  # ******************************************************************************************
  # END Plot 2
  # ******************************************************************************************
  # END plotting function
  ggsave(file=filename, path = file_directory , width=6, height=6)	
}

