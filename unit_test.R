#
# Unit tests
#

require(testthat)
source("Y:\\Data\\Questionnaire\\Scripts\\rsurvey\\likplot.R")
source("Y:\\Data\\Questionnaire\\Scripts\\rsurvey\\sankeyplot.R")
source("Y:\\Data\\Questionnaire\\Scripts\\rsurvey\\questfilt.R")

mydata <- read.delim("Y:\\Data\\Questionnaire\\unit_test\\System Safety Survey171.csv", sep = ';')

######################### Likert

# TC all
data_all = read.delim("Y:\\Data\\Questionnaire\\unit_test\\likertall.csv", sep = ',')
x = likplot(mydata)
write.csv(x,"Y:\\Data\\Questionnaire\\unit_test\\temp.csv")
result = read.delim("Y:\\Data\\Questionnaire\\unit_test\\temp.csv", sep = ',')
expect_equal(result,data_all)


# TC manager
data_all = read.delim("Y:\\Data\\Questionnaire\\unit_test\\likertmanager.csv", sep = ',')
mydata_filt = questfilt(mydata,'manager','all',FALSE)
x = likplot(mydata_filt)
write.csv(x,"Y:\\Data\\Questionnaire\\unit_test\\temp.csv")
result = read.delim("Y:\\Data\\Questionnaire\\unit_test\\temp.csv", sep = ',')
expect_equal(result,data_all)


# TC non manager
data_all = read.delim("Y:\\Data\\Questionnaire\\unit_test\\likertnonmanager.csv", sep = ',')
mydata_filt = questfilt(mydata,'non_manager','all',FALSE)
x = likplot(mydata_filt)
write.csv(x,"Y:\\Data\\Questionnaire\\unit_test\\temp.csv")
result = read.delim("Y:\\Data\\Questionnaire\\unit_test\\temp.csv", sep = ',')
expect_equal(result,data_all)



#################### Sankey
# TC all
q = "I know where to find information on instructions, guidelines, processes or templates"
data_all = read.delim("Y:\\Data\\Questionnaire\\unit_test\\sankeyall.csv", sep = ',')
mydata_filt = questfilt(mydata,'all','all',TRUE)
x = sankeyplot(mydata_filt,q)
write.csv(x,"Y:\\Data\\Questionnaire\\unit_test\\temp.csv")
result = read.delim("Y:\\Data\\Questionnaire\\unit_test\\temp.csv", sep = ',')
expect_equal(result,data_all)


# TC non manager
data_all = read.delim("Y:\\Data\\Questionnaire\\unit_test\\sankeynonmanager.csv", sep = ',')
mydata_filt = questfilt(mydata,'non_manager','all',TRUE)
x = sankeyplot(mydata_filt,q)
write.csv(x,"Y:\\Data\\Questionnaire\\unit_test\\temp.csv")
result = read.delim("Y:\\Data\\Questionnaire\\unit_test\\temp.csv", sep = ',')
expect_equal(result,data_all)



