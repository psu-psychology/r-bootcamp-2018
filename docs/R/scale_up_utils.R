lots_of_data <- function() {
  
  my_dir <- tempdir()
  rows <- 200
  
  random_sample_rt <- function(rows) {
    success <- FALSE
    while (isFALSE(success)) {
      # do something
      x <- rnorm(rows, 175, 100)
      # check for success
      success <- (x > 0)
    }
    return(x)
  }
  
  create_files <- function(x) {
    df <- data.frame(subject_nr = rep(sample(1:10000,1), rows),
                     block = rep(c('block1','block2'),rows/2),
                     pic = rep(c('pic1','pic2','pic3','pic4'), rows/4),
                     RT = random_sample_rt(rows)
    )
    temp <- paste0(x,'.csv')
    readr::write_csv(df, path = file.path(my_dir, temp))
  }
  
  for (i in seq_along(1:30)) {
    create_files(i)
  }
  
  return(my_dir)
}
