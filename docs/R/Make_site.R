# Make_site.R
#
# Updates and makes R bootcamp site

# Survey data (update first because later items depend on it)
source("R/Make_test_survey.R")
source("R/Get_bootcamp_googlesheet.R")
rmarkdown::render("talks/bootcamp-survey.Rmd", 
                  output_format = "html_document")

# Gilmore's talks
rmarkdown::render(input = "talks/bootcamp-day-1-intro.Rmd", 
                  output_format = "ioslides_presentation")

rmarkdown::render(input = "talks/slow-r.Rmd", 
                  output_format = c("html_document"))

rmarkdown::render(input = "talks/r-eproducible-science.Rmd", 
                  output_format = c("html_document"))

# Sample papja document
rmarkdown::render(input = "talks/gilmore-hallquist-bootcamp-2018-papaja.Rmd")

# Vallorani tutorial
rmarkdown::render(input = "talks/ggplot2_tutorial_vallorani.Rmd")

# Johnson tutorial
rmarkdown::render(input = "talks/ggplot2_tutorial_vallorani.Rmd")

# Hallquist talks
rmarkdown::render(input = "parallel_r.Rmd")
rmarkdown::render(input = "lavaan_tutorial.Rmd")
rmarkdown::render(input = "correlation_regression.Rmd")
rmarkdown::render(input = "dplyr_walkthrough_2018.Rmd")

# Ram talk
rmarkdown::render(input = "talks/IntroBasicEFA_2018_0815.Rmd")

# Albohn Talk
rmarkdown::render(input = "talks/scaling_up_Albohn.Rmd")

<<<<<<< HEAD
=======
# Better, but not tested
# Reduce(function(...) {rmarkdown::render}, list.files("talks/", pattern = ".Rmd$"))

>>>>>>> e8b76a6e89d6de580523855a8fa3c2d67676b368
# Render site last so that updated versions get copied to docs/
rmarkdown::render_site()
