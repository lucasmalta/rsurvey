#
# Unit tests
#

require(testthat)
source("Y:\\Data\\Questionnaire\\Scripts\\rsurvey\\likplot.R")
source("Y:\\Data\\Questionnaire\\Scripts\\rsurvey\\sankeyplot.R")

mydata <- read.delim("Y:\\Data\\Questionnaire\\unit_test\\System Safety Survey171.csv", sep = ';')

######################### Likert

# TC all
data_all = read.delim("Y:\\Data\\Questionnaire\\unit_test\\likertall.csv", sep = ',')
x = likplot(mydata, "all")
write.csv(x,"Y:\\Data\\Questionnaire\\unit_test\\temp.csv")
result = read.delim("Y:\\Data\\Questionnaire\\unit_test\\temp.csv", sep = ',')
expect_equal(result,data_all)


# TC manager
data_all = read.delim("Y:\\Data\\Questionnaire\\unit_test\\likertmanager.csv", sep = ',')
x = likplot(mydata, "manager")
write.csv(x,"Y:\\Data\\Questionnaire\\unit_test\\temp.csv")
result = read.delim("Y:\\Data\\Questionnaire\\unit_test\\temp.csv", sep = ',')
expect_equal(result,data_all)


# TC non manager
data_all = read.delim("Y:\\Data\\Questionnaire\\unit_test\\likertnonmanager.csv", sep = ',')
x = likplot(mydata, "non_manager")
write.csv(x,"Y:\\Data\\Questionnaire\\unit_test\\temp.csv")
result = read.delim("Y:\\Data\\Questionnaire\\unit_test\\temp.csv", sep = ',')
expect_equal(result,data_all)


#################### Sankey
# TC all
q = "I know where to find information on instructions, guidelines, processes or templates"
data_all = read.delim("Y:\\Data\\Questionnaire\\unit_test\\sankeyall.csv", sep = ',')
x = sankeyplot(mydata,q,'all')
write.csv(x,"Y:\\Data\\Questionnaire\\unit_test\\temp.csv")
result = read.delim("Y:\\Data\\Questionnaire\\unit_test\\temp.csv", sep = ',')
expect_equal(result,data_all)

# TC non manager
data_all = read.delim("Y:\\Data\\Questionnaire\\unit_test\\sankeynonmanager.csv", sep = ',')
x = sankeyplot(mydata,q,'non_manager')
write.csv(x,"Y:\\Data\\Questionnaire\\unit_test\\temp.csv")
result = read.delim("Y:\\Data\\Questionnaire\\unit_test\\temp.csv", sep = ',')
expect_equal(result,data_all)



