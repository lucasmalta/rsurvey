#
# Questionnaire data analysis
# Generates a Likert diverging stacked bar
# 
# Lucas Malta
# Carabiner 2018
#

require(dplyr)
require(googleVis)
require(plyr)
require(likert)
require(reshape2)
require(janitor)


likplot <- function(mydata, ismanager){
  
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
  
  
  ########### Likert diverging stacked bar graph
  
  
  ### Clean up data
  
  # Keep only the questions, remove other fileds
  drop_cols = c('Start.time','Completion.time','Email','Name','Please.state.your.Agile.team.s.name.','Do.you.have.any.comments.or.suggestions.regarding.System.Safety.activities.in.general.or.regarding.this.survey.','Are.you.in.one.of.these.roles..PO..PM..RTE..STE..Solution.Manager..Solution.Architect..System.Architect..SPE..Line.Manager.','What.System.Team.or.ART.do.you.belong.too.')
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
  
  # Append missing levels
  n.levels   <- sapply(data,nlevels)
  max.levels <- levels(data[,which.max(n.levels)])
  
  for (i in seq_along(data)) {
    mis.lev = which(!max.levels %in% levels(data[,i]))
    levels(data[,i]) = append(levels(data[,i]),max.levels[mis.lev])
  }
  
  # Order levels
  require(Epi)
  desired.order <- c('Strongly disagree','Disagree','Agree','Strongly agree')
  for (i in seq_along(data)) {
    data[,i] = Relevel(data[,i],desired.order)   # desired.order must be specified beforehand
  }
  
  # Generate graph and plot
  item1 <- likert(data)
  print(plot(item1) + theme(legend.text = element_text(size=15), axis.text=element_text(size=15)))
  return(data)
}