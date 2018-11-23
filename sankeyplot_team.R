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

sankeyplot_team <- function(mydata, question, ismanager){
  
  # Make sure we have the right ART names
  mydata = questfilt(mydata,'all','all','fix')
  
  # Remove repondants that disagree with System Safety is relevant to my tasks
  # and then, remove question as it adds no info for this specific analysis.
  # We also need to fix levels
  mydata = subset(mydata, System.Safety.is.relevant.to.my.tasks.=='Agree')
  mydata = mydata[ , !(names(mydata) %in% 'System.Safety.is.relevant.to.my.tasks.')]
  mydata = droplevels(mydata)
  
  # Keep ART names
  drop_cols = c('Start.time','Completion.time','Email','Name','What.System.Team.or.ART.do.you.belong.too.','Do.you.have.any.comments.or.suggestions.regarding.System.Safety.activities.in.general.or.regarding.this.survey.','Are.you.in.one.of.these.roles..PO..PM..RTE..STE..Solution.Manager..Solution.Architect..System.Architect..SPE..Line.Manager.')
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
  
  # Stack data per team name
  stack_team <- melt(data, id = c("Please.state.your.Agile.team.s.name."))
  
  # Filter data keeping only one question
  stack_team_filtered= filter(stack_team, variable==question)
  
  # Rename "value" to avoid masking with internal R variables
  stack_team_filtered <- rename(stack_team_filtered, c(value = "val"))
  
  # Generate a table with percentage of data in each level
  sankeydata = stack_team_filtered %>% 
    tabyl(Please.state.your.Agile.team.s.name., val) %>% 
    adorn_percentages("row") 
  
  # Stack teams again
  sankeydata_stack_team <- melt(sankeydata, id = c("Please.state.your.Agile.team.s.name."))
  
  # Generate graph and plot
  sk1 <- gvisSankey(sankeydata_stack_team, from="value", to="variable", weight="value")
  print(plot(sk1))
  
  return(sankeydata_stack_team)
}