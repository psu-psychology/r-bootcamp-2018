# Make_test_survey.R
# Timestamp,R_exp,Banjo,Psych_age_yrs,Sleep_hrs,Fav_day,Crisis
#8/11/2017 10:13:58,Pro,9,54,8,1/17/2018,Yes

n_subs = 50

# Duplicate Timestamps
now <- Sys.time()
Timestamp <- rep(now, n_subs)

# Psych_age_yrs uniform on [22,55] rounded to nearest integer
Psych_age_yrs <- round(runif(n_subs, min = 12, max = 85), 0)

# Sleep_hrs normal on 8 with SD = 1?
Sleep_hrs <- rnorm(n_subs, mean = 8, sd = 1)

# R_exp equally distributed across 5 categories
exp_levels <- c("none", "limited", "some", "lots", "pro")
R_exp <- rep(exp_levels, n_subs)
R_exp <- R_exp[sample(1:n_subs, replace = FALSE)]

# Banjo enthusiasm lower in older, higher in lower sleep
# beta_age <- -.2
# beta_sleep <- -.5
# mean_banjo <- 5
# banjo_no_intercept <- rep(mean_banjo, n_subs) + beta_age*Psych_age_yrs + beta_sleep*Sleep_hrs
# banjo_intercept <- 1-min(banjo_no_intercept)

Banjo <- round(runif(n_subs, 1, 10), 0)

# Tidy_data random
Crisis <- rep(c("Yes, significant", "Yes, slight", "No", "Don't know"), n_subs)
Crisis <- Crisis[sample(1:n_subs, replace = TRUE)]

# Fav_date
Fav_date <- rep(Sys.Date(), n_subs)

survey <- data.frame(Timestamp, R_exp, Banjo, Psych_age_yrs, 
                     Sleep_hrs, Fav_date, Crisis)
write.csv(survey, "data/survey_test.csv", row.names = FALSE)