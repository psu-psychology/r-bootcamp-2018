# Rick's R Bootcamp work 2018-08-16

# Parameters
mean_1 <- 0
mean_2 <- 3
sd_1 <- 1
sd_2 <- 1
samples_1 <- 100
samples_2 <- 100

# Generate 0-centered, and non-zero-centered normally distributed data
(data_mean_zero <- rnorm(n = 100, mean = 0, sd = 1))
(data_mean_nonzero <- rnorm(n = 100, mean = 3, sd = 1))

# Plot histograms
hist(data_mean_zero)
hist(data_mean_nonzero)

# Run t-tests
t.test(data_mean_zero)
t.test(data_mean_nonzero)

# Putting this all in a function
my_hist_t <- function(my_samples = 100, my_mean = 0, my_sd = 1) {
  my_data <- rnorm(n = my_samples, mean = my_mean, sd = my_sd)
  hist(my_data)
  t.test(my_data)
}