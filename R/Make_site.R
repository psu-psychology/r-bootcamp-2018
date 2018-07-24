# Make_site.R
#
# Updates and makes R bootcamp site
#

# render ioslides_presentation last because github_document makes HTML
rmarkdown::render("talks/r-eproducible-science.Rmd", 
                  output_format = c("github_document", 
                                    "pdf_document", 
                                    "word_document", 
                                    "ioslides_presentation"))
# Survey data
source("R/Update_survey.R")
source("R/Make_test_survey.R")
rmarkdown::render("talks/bootcamp-survey.Rmd",
                  output_format = c("github_document", 
                                    "pdf_document", 
                                    "word_document", 
                                    "ioslides_presentation"))

# Sample papja document
rmarkdown::render("papaja_demo/gilmore-lebreton-hallquist.Rmd")

# James talk
rmarkdown::render("talks/R-Workshop-James.Rmd",
                  output_format = c("github_document",
                                    "pdf_document",
                                    "word_document",
                                    "ioslides_presentation"))

# Render site last so that updated versions get copied to docs/
rmarkdown::render_site()
