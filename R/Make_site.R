# Make_site.R
#
# Updates and makes R bootcamp site

# Gilmore's talks
rmarkdown::render(input = "talks/bootcamp-day-1-intro.Rmd", 
                  output_format = "ioslides_presentation")

rmarkdown::render(input = "talks/slow-r.Rmd", 
                  output_format = c("html_document"))

rmarkdown::render(input = "talks/r-eproducible-science.Rmd", 
                  output_format = c("html_document"))

# Survey data
source("R/Update_survey.R")
source("R/Make_test_survey.R")
rmarkdown::render(input = "talks/bootcamp-survey.Rmd",
                  output_format = c("pdf_document", 
                                    "html_document"))

# Sample papja document
rmarkdown::render(input = "talks/gilmore-hallquist-bootcamp-2018-papaja.Rmd")

# Render site last so that updated versions get copied to docs/
rmarkdown::render_site()
