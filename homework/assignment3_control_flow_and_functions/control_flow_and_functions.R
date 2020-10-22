
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

# Problem 2 ---------------------------------------------------------------
# Create a fibonacci sequence function
#1 create a fib function
fib <- function(n, start_from=c(0)){
  if (n >= 0){
    n <- round(n) # if a decimal is passed e.g n = 3.4, the function will break
    ifelse(start_from == c(0), count_up <- 0, count_up <- 1)
    fib_seq <- start_from
    while (count_up != n){
      fib_num <- fib_seq[count_up]
      count_up <- count_up + 1
      if (length(fib_num) == 0){ 
        # if we are at 1 the sequence variable hasn't been populated enough to
        # pull 2 values from it so we set the number manually
        fib_num = 1
      } else {
        fib_num <- fib_num + fib_seq[count_up]
      }
      fib_seq <- append(fib_seq, fib_num)
    }
    fib_seq
  } else {
    warning("N must be 0 or a positive integer")
  }
}


# Problem 3 ---------------------------------------------------------------


