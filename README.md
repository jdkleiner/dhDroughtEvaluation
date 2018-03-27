# dhDroughtEvaluation-R
Drought Stage Determination Code for Drupal VAHydro

This repo will house R scripts for retrieving NWIS historic gage data, processing the data to estimate drought stage, output data to VAHydro as drupal properties 

11/17/17
	-gwl code still gets stuck when running from server! 

	11/21/17, 11/22/17
	-gwl code appears to have run for all gages 11/14/2017 - 11/20/2017
	
	11/29/17
	-something strange happened with both gwl and flow 
	--propvalues = 0 and no propcode set 
	--problem appears to have been corrected the following day 
	
	12/8/17
	-gwl and flow look good
	
	12/13/17
	-flow only ran for 4 gages 
	
	12/20/17
	-gwl and flow look good

	1/19/18
	-need to update to use REST functions 
	
	2.1.18
	-need to update MLLR code to handle "Ice" 
	
	3.6.18
	-merged ICE and other NO DATA handling 
	
	3.23.18
	-gwl and flow look good
	
	3.27.18
	-gwl and flow look good