
# -------------------------------------------------------------------------
# Author: Sam Farmer
# Date: 10/18/2020
#
#  This assignment explores control flow and writing functions.
# -------------------------------------------------------------------------

# setup -------------------------------------------------------------------

rm(list = ls())

# Problem 1 ---------------------------------------------------------------
# Recreate sum, mean, and var functions
#1 create a 'my_sum' function that mimics sum
my_sum <- function(x, na.rm=TRUE){
 if (length(x) == 0){
   warning("X must have length greater than 0")
   return(0)
 }
  if ((typeof(x) == "integer") || (typeof(x) == "double")){
    if (na.rm){
      x <- x[!is.na(x)]
    }
    sum <- 0
    for (value in x){
      sum = sum + value
    }
    return(sum)
  }
  else{
    warning("x must be vector of double or integer type")
  }
}

#2 create a my_mean function based off of my_sum
my_mean <- function(x, na.rm=TRUE){
  sum = my_sum(x, na.rm)
  denom = ifelse(na.rm, length(x[!is.na(x)]), length(x))
  sum / denom
}

#3 create a my_var function based off of my_mean and my_sum
my_var <- function(x, na.rm=TRUE){
  mean <- my_mean(x, na.rm)
  sum_square_x <- my_sum((x - mean)^2, na.rm)
  denom <- ifelse(na.rm, length(x[!is.na(x)]), length(x))
  sum_square_x /(denom - 1)
}
