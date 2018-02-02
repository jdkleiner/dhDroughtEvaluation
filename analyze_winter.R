## Code to calculate the current drought condition estimates and create a graph to show all estimates by month and threshold 
# This code will be updated using "chron" every March 1st

#file_directory <- "/var/www/html/images/dh"  
file_directory <- "C:\\Users\\nrf46657\\Desktop\\VAHydro Development\\GitLab\\VAHydro\\dh_drought\\src\\r\\"
base_url <- 'http://deq1.bse.vt.edu/d.dh' #needed for REST functions

#fid needed for retrieving beta properties via REST
fid <- 58623 #fid <- 58655 
target_year=2018
gage <- c(02016000)  #gage <- c(02039500) 

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
source(paste(file_directory,"rest_functions.R", sep = ""))
source(paste(file_directory,"./rest_auth.private",sep=""));
token <- rest_token (base_url, token, rest_uname = rest_uname, rest_pw = rest_pw) #token needed for REST

for (i in 1:length(gage)) {

	# Initialize variables
	n_f_flow <- c()
	year <- year(Sys.Date())

        # @todo: this looks like it is no longer used
	# Find the element id for the USGS gage
	#elem_url <- paste("http://deq1.bse.vt.edu/om/remote/get_modelData.php?variables=elementid,elemname&scenarioid=17&querytype=custom2&operation=7&custom1=usgs_stream_gage&params=", gage[i], sep="")
	#elem_info <- scan(elem_url, what="character", sep=",")
	#elid <- elem_info[3]


	# Read flow for entire record to gather all historical flow data
	url_base <- "https://waterservices.usgs.gov/nwis/dv/?site=";
	url <- paste(url_base, gage[i], "&variable=00060&format=rdb&startDT=1838-01-01", sep="")	
	data <- try(read.table(url, skip = 2, comment.char = "#", header=TRUE))

		# Continuing with no data available
		if (class(data)=="try-error") { 
                  print(paste("NWIS Responded NO DATA for", url, sep=''));
                  next
		} 

		# Remove gages that don't even have a column for flow data on USGS
			if (ncol(data) < 4) {
				print(paste("NWIS Responded NO DATA for", url, sep=''));
				next
			}

	historic <- data[-1,] # delete row that has no significance for this calculation
	
	# Find first and last date on record
	start.date <- as.Date(as.character(historic[2,3]))
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
		f <- historic[,4][(as.Date(historic$datetime)>=StartDate) & (as.Date(historic$datetime)<=EndDate)]
			
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
	values <- c(most_recent, median)
	lines <- data.frame(names, values)

	plot_xtitle <- "Winter Flow, cfs"
	plot_xsubtitle <- "Average of daily mean flows from November to February of each year on record"

	# Saving file to the correct location
	filename <- paste("usgs", gage[i], "mllr_bar_winterflows", target_year, ".png", sep="_")

	# Create histogram/density plot to show recent winter flow vs historic winter flows
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

	#--------------------------------------------------------------------------
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
	print(beta_table)

	
#Add MLLR Curves to Plot
	P_est <- function(b0, b1, x) {1/(1+exp(-(b0 + b1*x)))}

	#remove any rows with "Ice" values for locating maximum historic flow value 
	if (length(which(historic[,4]== "Ice")) != 0 ){
	
	  historic_no_ice <- historic[-which(historic[,4]== "Ice"),]
	} else {
	  historic_no_ice <- historic
  }

	#need to determine x-range to use for plotting mllr curves, currently set to go from zero to 95th percentile flow
	#will cause number of histogram bins to be different than they were before
	xmax <- as.numeric(as.character(quantile(as.numeric(as.character(historic_no_ice[,4])),0.95)))
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
plt<-plt+scale_y_continuous(sec.axis = sec_axis(~./0.1, name = "Probability Estimate"),breaks= pretty_breaks())

# END plotting function
ggsave(file=filename, path = file_directory , width=6, height=6)	
}

