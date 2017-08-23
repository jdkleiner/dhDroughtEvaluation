library(httr);

dh_rest_token <- function(site, token){
  
  #Cross-site Request Forgery Protection (Token required for POST and PUT operations)
  csrf_url <- paste(site,"/restws/session/token/",sep="");
  csrf <- GET(url=csrf_url,authenticate("FANCYUSERNAME","FANCYPASSWORD"));
  token <- content(csrf);
  
} #close function