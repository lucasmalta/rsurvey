#
# Unit tests
#

require(testthat)
source("Y:\\Data\\Questionnaire\\Scripts\\rsurvey\\likplot.R")
source("Y:\\Data\\Questionnaire\\Scripts\\rsurvey\\sankeyplot.R")
source("Y:\\Data\\Questionnaire\\Scripts\\rsurvey\\questfilt.R")

mydata <- read.delim("Y:\\Data\\Questionnaire\\unit_test\\System Safety Survey171.csv", sep = ';')
q = "What.System.Team.or.ART.do.you.belong.too."

######################### Likert


test_that("T1", {
  # TC all
  data_all = read.delim("Y:\\Data\\Questionnaire\\unit_test\\likertall.csv", sep = ',')
  x = likplot(mydata, q)
  write.csv(x,"Y:\\Data\\Questionnaire\\unit_test\\temp.csv")
  result = read.delim("Y:\\Data\\Questionnaire\\unit_test\\temp.csv", sep = ',')
  expect_equal(result,data_all)
})

# TC manager
test_that("T2", {
  data_all = read.delim("Y:\\Data\\Questionnaire\\unit_test\\likertmanager.csv", sep = ',')
  mydata_filt = questfilt(mydata,'manager','all','keep')
  x = likplot(mydata_filt, q)
  write.csv(x,"Y:\\Data\\Questionnaire\\unit_test\\temp.csv")
  result = read.delim("Y:\\Data\\Questionnaire\\unit_test\\temp.csv", sep = ',')
  expect_equal(result,data_all)
})


# TC non manager
test_that("T3", {
  data_all = read.delim("Y:\\Data\\Questionnaire\\unit_test\\likertnonmanager.csv", sep = ',')
  mydata_filt = questfilt(mydata,'non_manager','all','keep')
  x = likplot(mydata_filt, q)
  write.csv(x,"Y:\\Data\\Questionnaire\\unit_test\\temp.csv")
  result = read.delim("Y:\\Data\\Questionnaire\\unit_test\\temp.csv", sep = ',')
  expect_equal(result,data_all)
})


#################### Sankey
# TC all
test_that("T4", {
  q = "I.know.where.to.find.information.on.instructions..guidelines..processes.or.templates.to.perform.my.tasks.that.are.System.Safety.relevant."
  data_all = read.delim("Y:\\Data\\Questionnaire\\unit_test\\sankeyall.csv", sep = ',')
  x = sankeyplot(mydata,q)
  write.csv(x,"Y:\\Data\\Questionnaire\\unit_test\\temp.csv")
  result = read.delim("Y:\\Data\\Questionnaire\\unit_test\\temp.csv", sep = ',')
  expect_equal(result,data_all)
})


# TC non manager
test_that("T5", {
  data_all = read.delim("Y:\\Data\\Questionnaire\\unit_test\\sankeynonmanager.csv", sep = ',')
  q = "I.know.where.to.find.information.on.instructions..guidelines..processes.or.templates.to.perform.my.tasks.that.are.System.Safety.relevant."
  mydata_filt = questfilt(mydata,'non_manager','all','fix')
  x = sankeyplot(mydata_filt,q)
  write.csv(x,"Y:\\Data\\Questionnaire\\unit_test\\temp.csv")
  result = read.delim("Y:\\Data\\Questionnaire\\unit_test\\temp.csv", sep = ',')
  expect_equal(result,data_all)
})


