#
# Questionnaire data analysis
# Generates a Sankey plot
# 
# Lucas Malta
# Carabiner 2018
#

require(dplyr)
require(googleVis)
require(plyr)
require(reshape2)
require(janitor)
source("Y:\\Data\\Questionnaire\\Scripts\\rsurvey\\questfilt.R")

sankeyplot <- function(mydata, question, ismanager){
  
  # Make sure we have the right ART names
  mydata = questfilt(mydata,'all','all','fix')
  
  # Remove repondants that disagree with System Safety is relevant to my tasks
  # and then, remove question as it adds no info for this specific analysis.
  # We also need to fix levels
  mydata = subset(mydata, System.Safety.is.relevant.to.my.tasks.=='Agree')
  mydata = mydata[ , !(names(mydata) %in% 'System.Safety.is.relevant.to.my.tasks.')]
  mydata = droplevels(mydata)
  
  # Keep ART names
  drop_cols = c('Start.time','Completion.time','Email','Name','Please.state.your.Agile.team.s.name.','Do.you.have.any.comments.or.suggestions.regarding.System.Safety.activities.in.general.or.regarding.this.survey.','Are.you.in.one.of.these.roles..PO..PM..RTE..STE..Solution.Manager..Solution.Architect..System.Architect..SPE..Line.Manager.')
  data = mydata[ , !(names(mydata) %in% drop_cols)]
  
  # Fix levels
  data[1:8] <- lapply(data[1:8], factor, levels=c('Strongly disagree','Disagree','Agree','Strongly agree'))
  
  # Stack data per team name
  stack_team <- melt(data, id = c("What.System.Team.or.ART.do.you.belong.too."))
  
  # Filter data keeping only one question
  stack_team_filtered= filter(stack_team, variable==question)
  
  # Rename "value" to avoid masking with internal R variables
  stack_team_filtered <- rename(stack_team_filtered, c(value = "val"))
  
  # Generate a table with percentage of data in each level
  sankeydata = stack_team_filtered %>% 
    tabyl(What.System.Team.or.ART.do.you.belong.too., val) %>% 
    adorn_percentages("row") 
  
  # Stack teams again
  sankeydata_stack_team <- melt(sankeydata, id = c("What.System.Team.or.ART.do.you.belong.too."))
  
  # Generate graph and plot
  sk1 <- gvisSankey(sankeydata_stack_team, from="value", to="variable", weight="value")
  print(plot(sk1))
  
  return(sankeydata_stack_team)
}