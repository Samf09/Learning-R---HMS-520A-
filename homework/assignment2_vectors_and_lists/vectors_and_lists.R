# -------------------------------------------------------------------------
# Author: Sam Farmer
# Date 10/10/2020
# Purpose: This assignment explores how to create vectors and lists using 
# built in R functions

# -------------------------------------------------------------------------
#Setup
rm(list = ls())

# problem 1 ---------------------------------------------------------------
# Create various vectors
vector1 <- c(1:100)
vector2 <- rep(10, 100)
vector3 <- rep(c(1, 2, 3, 4, 5), 10)
vector4 <- rep(c(1, 2, 3, 4, 5), each=10)
vector5 <- seq.int(0.00, 1.00, 0.01)


# problem 2 ---------------------------------------------------------------
# Rivers dataset practice
help(rivers)
paste0("type of rivers data set is an atomic vector of: ", typeof(rivers))

# Create an atomic vector of Rivers that contains contains the length, sum,
# mean, median, variance, standard deviation, minimum and maximum of 
# log-transformed rivers
log_statistics <- function(data){
  log_data <- log(data)
  data_len <- length(log_data)
  data_sum <- sum(log_data)
  data_mean <- mean(log_data)
  data_median <- median(log_data)
  data_var <- var(log_data)
  data_stddev <- sd(log_data)
  data_min <- min(log_data)
  data_max <- max(log_data)
  data_stats <- c("length"= data_len, "sum"= data_sum,
                  "mean"= data_mean, "median"= data_median,
                  "variance"= data_var, "stddev"= data_stddev,
                  "min"= data_min, "max"= data_max)
  return(data_stats)
}

log_river_stats <- log_statistics(rivers)
# Remove the 10 least and 10 greatest values in Rivers then create descriptive
# stats vector of it
log_trimriver_stats <- log_statistics(sort(rivers)[11:(length(rivers)-10)])

