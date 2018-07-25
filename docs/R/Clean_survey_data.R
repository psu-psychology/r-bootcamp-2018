Clean_survey_data <- function(df) {
  # Ensure standard names
  names(df) <- c("Timestamp",
                  "R_exp",
                  "GoT",
                  "Age_yrs",
                  "Sleep_hrs",
                  "Fav_day",
                  "Tidy_data")
  
  # Ensure numbers for GoT, Age_yrs, Sleep_hrs
  df$GoT <- readr::parse_number(df$GoT)
  df$Age_yrs <- readr::parse_number(df$Age_yrs)
  df$Sleep_hrs <- readr::parse_number(df$Sleep_hrs)
  
  # Drop NAs and report number
  drops <- df[!complete.cases(df),]
  message(sprintf("Dropping %d case(s) with NAs.\n", dim(drops)[1]))
  df <- df[complete.cases(df),]
  df
}