#
# Questionnaire data analysis
# Filter data
# 
# Lucas Malta
# Carabiner 2018
#

questfilt <- function(mydata, ismanager, art, setother){
  
  # Filter data concerning management
  if(ismanager=='manager'){
    mydata = subset(mydata, Are.you.in.one.of.these.roles..PO..PM..RTE..STE..Solution.Manager..Solution.Architect..System.Architect..SPE..Line.Manager.=='Yes')
  }
  else if (ismanager=='non_manager')
  { 
    mydata = subset(mydata, Are.you.in.one.of.these.roles..PO..PM..RTE..STE..Solution.Manager..Solution.Architect..System.Architect..SPE..Line.Manager.=='No')
  }
  else if (ismanager=='all'){}
  else
  {
    stop("ERROR: Wrong manager syntax. Allowed: manager, non_manager, all")
  }
  
  
  # Fix levels manually
  levels(mydata$What.System.Team.or.ART.do.you.belong.too.) <- tolower(levels(mydata$What.System.Team.or.ART.do.you.belong.too.))
  levels(mydata$What.System.Team.or.ART.do.you.belong.too.)[levels(mydata$What.System.Team.or.ART.do.you.belong.too.)=="adas to w48, thereafter ad"] <- "ADAS"
  levels(mydata$What.System.Team.or.ART.do.you.belong.too.)[levels(mydata$What.System.Team.or.ART.do.you.belong.too.)=="adhoc"] <- "VMC"
  
  
  # Convert all related levels to the same name
  ad_options = grep('\\<ad\\>|ad |autonomous|adfsp|adf&sp', levels(mydata$What.System.Team.or.ART.do.you.belong.too.), value = TRUE)
  adas_options = grep('advanced driving |advanced driver |adas|active safety|rsds', levels(mydata$What.System.Team.or.ART.do.you.belong.too.), value = TRUE)
  protective_options = grep('protective|prosaf|passive saftey', levels(mydata$What.System.Team.or.ART.do.you.belong.too.), value = TRUE)
  vmc_options = grep('motion and control|vehicle motion|vmc|vemc', levels(mydata$What.System.Team.or.ART.do.you.belong.too.), value = TRUE)
  
  for (i in ad_options){
    levels(mydata$What.System.Team.or.ART.do.you.belong.too.)[levels(mydata$What.System.Team.or.ART.do.you.belong.too.)==i] <- "AD"
  }
  for (i in adas_options){
    levels(mydata$What.System.Team.or.ART.do.you.belong.too.)[levels(mydata$What.System.Team.or.ART.do.you.belong.too.)==i] <- "ADAS"
  }
  for (i in protective_options){
    levels(mydata$What.System.Team.or.ART.do.you.belong.too.)[levels(mydata$What.System.Team.or.ART.do.you.belong.too.)==i] <- "PROTECTIVE"
  }
  for (i in vmc_options){
    levels(mydata$What.System.Team.or.ART.do.you.belong.too.)[levels(mydata$What.System.Team.or.ART.do.you.belong.too.)==i] <- "VMC"
  }
  
  # Set other levels to Other
  if (setother=='fix'){
    w = levels(mydata$What.System.Team.or.ART.do.you.belong.too.)
    w[!(levels(mydata$What.System.Team.or.ART.do.you.belong.too.) %in% c('ADAS','AD','PROTECTIVE','VMC'))] <- 'Other'
    levels(mydata$What.System.Team.or.ART.do.you.belong.too.) <- w
  }
  else if (setother=='keep'){}
  else if (setother=='del'){
    w = levels(mydata$What.System.Team.or.ART.do.you.belong.too.)
    w[!(levels(mydata$What.System.Team.or.ART.do.you.belong.too.) %in% c('ADAS','AD','PROTECTIVE','VMC'))] <- 'Other'
    levels(mydata$What.System.Team.or.ART.do.you.belong.too.) <- w
    mydata = subset(mydata, mydata$What.System.Team.or.ART.do.you.belong.too. != 'Other')
  }
  
  # Filter data concerning ART
  if(art=='ad'){
    mydata = subset(mydata, What.System.Team.or.ART.do.you.belong.too.=='AD')
  }
  else if (art=='adas')
  { 
    mydata = subset(mydata, What.System.Team.or.ART.do.you.belong.too.=='ADAS')
  }
  else if (art=='protective')
  { 
    mydata = subset(mydata, What.System.Team.or.ART.do.you.belong.too.=='PROTECTIVE')
  }
  else if (art=='vmc')
  { 
    mydata = subset(mydata, What.System.Team.or.ART.do.you.belong.too.=='VMC')
  }
  else if (art=='all'){}
  else
  {
    stop("ERROR: Wrong syntax. Allowed: ad, adas, protective, vmc")
  }
  
  mydata = droplevels(mydata)
  return(mydata)
  
}