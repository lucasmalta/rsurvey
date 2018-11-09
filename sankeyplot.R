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

sankeyplot <- function(mydata, question, ismanager){
  
  # Filter data
  if(ismanager=='manager'){
    managers = subset(mydata, Are.you.in.one.of.these.roles..PO..PM..RTE..STE..Solution.Manager..Solution.Architect..System.Architect..SPE..Line.Manager.=='Yes')
    mydata = managers
  }
  else if (ismanager=='non_manager')
  { 
    non_managers = subset(mydata, Are.you.in.one.of.these.roles..PO..PM..RTE..STE..Solution.Manager..Solution.Architect..System.Architect..SPE..Line.Manager.=='No')
    mydata = non_managers
  }
  else if (ismanager=='all'){}
  else
  {
    stop("ERROR: Wrong manager syntax. Allowed: manager, non_manager, all")
  }
  
  # Remove repondants that disagree with System Safety is relevant to my tasks
  # and then, remove question as it adds no info for this specific analysis.
  # We also need to fix levels
  mydata = subset(mydata, System.Safety.is.relevant.to.my.tasks.=='Agree')
  mydata = mydata[ , !(names(mydata) %in% 'System.Safety.is.relevant.to.my.tasks.')]
  mydata = droplevels(mydata)
  
  # Keep ART names
  drop_cols = c('Start.time','Completion.time','Email','Name','Please.state.your.Agile.team.s.name.','Do.you.have.any.comments.or.suggestions.regarding.System.Safety.activities.in.general.or.regarding.this.survey.','Are.you.in.one.of.these.roles..PO..PM..RTE..STE..Solution.Manager..Solution.Architect..System.Architect..SPE..Line.Manager.')
  data = mydata[ , !(names(mydata) %in% drop_cols)]
  
  # Rename column names
  data <- rename(data, c(
    I.have.the.necessary.knowledge.on.System.Safety.to.perform.my.tasks. = "I have the necessary knowledge on System Safety to perform my tasks", 
    I.feel.that.the.organization.demonstrates.the.importance.of.System.Safety.activities..e.g..Systems.Safety.is.prioritized.in.planning.and.execution..resources.are.selected.properly..proper.follow.u... = "I feel that the organization demonstrates the importance of System Safety", 
    I.know.how.to.find.experts.to.support.me.on.System.Safety. = "I know how to find experts to support me on System Safety", 
    I.know.where.to.find.information.on.instructions..guidelines..processes.or.templates.to.perform.my.tasks.that.are.System.Safety.relevant. = "I know where to find information on instructions, guidelines, processes or templates", 
    It.is.clear.to.me.how.the.available.System.Safety.instructions..guidelines..processes.and.templates.shall.be.used. = "It is clear to me how the available System Safety instructions, guidelines, processes and templates shall be used.", 
    I.am.responsible.to.contribute.to.System.Safety.related.activities. = "I am responsible to contribute to System Safety related activities.", 
    I.clearly.understand.what.I.need.to.deliver.in.regards.to.System.Safety. = "I clearly understand what I need to deliver in regards to System Safety.", 
    My.team.s.System.Safety.related.activities.are.completed.on.time. = "My team's System Safety related activities are completed on time." 
  ))
  
  # Fix levels manually
  levels(data$What.System.Team.or.ART.do.you.belong.too.) <- tolower(levels(data$What.System.Team.or.ART.do.you.belong.too.))
  levels(data$What.System.Team.or.ART.do.you.belong.too.)[levels(data$What.System.Team.or.ART.do.you.belong.too.)=="adas to w48, thereafter ad"] <- "ADAS"
  
  # Convert all related levels to the same name
  ad_options = grep('\\<ad\\>|ad |autonomous|adfsp', levels(data$What.System.Team.or.ART.do.you.belong.too.), value = TRUE)
  adas_options = grep('advanced driving |advanced driver |adas|active safety', levels(data$What.System.Team.or.ART.do.you.belong.too.), value = TRUE)
  protective_options = grep('protective|prosaf', levels(data$What.System.Team.or.ART.do.you.belong.too.), value = TRUE)
  vmc_options = grep('motion and control|vehicle motion|vmc', levels(data$What.System.Team.or.ART.do.you.belong.too.), value = TRUE)
  
  for (i in ad_options){
    levels(data$What.System.Team.or.ART.do.you.belong.too.)[levels(data$What.System.Team.or.ART.do.you.belong.too.)==i] <- "AD"
  }
  for (i in adas_options){
    levels(data$What.System.Team.or.ART.do.you.belong.too.)[levels(data$What.System.Team.or.ART.do.you.belong.too.)==i] <- "ADAS"
  }
  for (i in protective_options){
    levels(data$What.System.Team.or.ART.do.you.belong.too.)[levels(data$What.System.Team.or.ART.do.you.belong.too.)==i] <- "PROTECTIVE"
  }
  for (i in vmc_options){
    levels(data$What.System.Team.or.ART.do.you.belong.too.)[levels(data$What.System.Team.or.ART.do.you.belong.too.)==i] <- "VMC"
  }
  
  # Set other levels to Other
  w = levels(data$What.System.Team.or.ART.do.you.belong.too.)
  w[!(levels(data$What.System.Team.or.ART.do.you.belong.too.) %in% c('ADAS','AD','PROTECTIVE','VMC'))] <- 'Other'
  levels(data$What.System.Team.or.ART.do.you.belong.too.) <- w
  
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