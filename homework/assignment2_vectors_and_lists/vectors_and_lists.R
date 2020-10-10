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
# stats vector from it
log_trimriver_stats <- log_statistics(sort(rivers)[11:(length(rivers)-10)])


# Problem 3 ---------------------------------------------------------------
# Create a list and modify its contents
# 1 create a list 'u' with two items x = c(5, 6, 7, 8) and 
# y = c("a", "b", "c", "d")
u <- list(x = c(5, 6, 7, 8), y = c("a", "b", "c", "d"))

# 2
# Modify y in u such that it has numerical values c(1, 2, 3, 4).
u$y <- c(1, 2, 3, 4)

# 3
# What is the best way to compute the mean of all elements in x and y?
mean_u <- lapply(u, mean) # compute mean for each vector within list
mean_u_allvals <- mean(unlist(u)) # compute mean for all values in list

# 4 
# Add x2 = x^2 and log_x = log(x) into the list.
u$"x^2" <- u$x^2
u$"log_x" <- log(u$x)

# 5
# Remove log_x from the list.
u$log_x <- NULL
