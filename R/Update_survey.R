# Update_survey.R
#
# Updates Googlesheet survey data and generates new R Markdown report
#

source("R/Get_bootcamp_googlesheet.R")
rmarkdown::render("talks/bootcamp-survey.Rmd")