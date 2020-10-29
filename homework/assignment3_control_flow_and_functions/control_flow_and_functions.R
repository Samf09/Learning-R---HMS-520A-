
# -------------------------------------------------------------------------
# Author: Sam Farmer
# Date: 10/18/2020
#
#  This assignment explores control flow and writing functions by recreating
#  built in R functions such as sum, and mean. Along with creating some new
#  functions also
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
#2 add a start_from argument
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
    warning("N must be 0, or a positive integer")
  }
}


# Problem 3 ---------------------------------------------------------------
# Create unique related functions
#1 create a function count that returns how many times a passed argument is
# in a vector
#2 update function so that x can be a vector and to return vector of counts
count <- function(vec, x){
  if (typeof(vec) == "integer" || typeof(vec) == "double" ||
      typeof(vec) == "character"){
    return_count <- vector()
    for (i in 1:length(x)){
      subx <- x[i]
      count <- 0
      for (i in 1:length(vec)){
        subvec <- vec[i]
        if(subvec == subx){
          count = count + 1
        }
      }
      return_count <- append(return_count, count)
      }
    return(return_count)
  } else{
    warning("vec must be an atomic vector")
  }
}

#3 create a my_unique function that returns unique values in a vector and counts
# if told to
my_unique <- function(vec, return_counts = FALSE){
  if (return_counts){
    x <- unique(vec)
    counts <- count(vec, x)
    x_counts <- data.frame(x = x,
                           counts = counts)
    return(x_counts)
  } else{
    unique(vec)
  }
}

# problem 4 ---------------------------------------------------------------
# Create functions to help with binomial data computation
#1 create a binomial_fun function with arguments s (sum of events) and n (# events)
# Create a function binomial_fun with argument s (the sum of the events) and 
# n (samples) sizes, and return the estimation of mean $\hat{p}$ and the
# standard deviation of the estimation $\sqrt{\hat{v}}$. Notice that s and
# n could be vectors. You could assume they have the same length for this part
binomial_fun <- function(s, n){
  s <- sum(s)
  phat <- (s/n)
  # browser()
  std_dev <- sqrt((phat * (1-phat))/n)
  return(c("mean_phat"= mean(phat), "std_dev"= std_dev))
}

# problem 5 ---------------------------------------------------------------
# Create a polynomial fit function
#1 Write a function called lin_fit, with x and y as the arguments, and return 
# the coefficients of the linear fit. You could use lm function, and the 
# coefficients should have length 2 (intercept and slope).
lin_fit <- function(x, y, data){
  lm(as.formula(paste0(y, " ~ ", x)), data)
}

#2 Extend the function create `poly_fit`, with `x` and `y` and `degree = 1` as 
# the arguments and return coefficients of the polynomial fit. For example if 
# `degree = 3`
poly_fit <- function(x, y, degree = 1){
  lm_equation <- paste0(y, " ~ 1 +")
  for (num in 1:degree){
    if (num == 1){
      lm_equation <- paste0(lm_equation, " ", x)
    } else {
      lm_equation <- paste0(lm_equation, " + ", x, "^", num)
    }
  }
  #lm(as.formula(paste0(lm_equation)), data = data)
  paste0(lm_equation)
}

#3 Create a poly_pred function, with x and coef as the arguments. This function
# will take x and the polynomial coefficients and return the dependent vectors.
#(Hint: we could automatically infer the degree by the length of coef.)
poly_pred <- function(x, coef){
  degree = length(coef)
  lm_equation <- paste0(" ~ 1 + ")
  for (num in 1:degree){
    if (num == 1){
      lm_equation <- paste0(lm_equation, x)
    } else {
      lm_equation <- paste0(lm_equation, " + ", x, "^", num)
    }
  }
  # I split the string that represents my polynomial on everything that isn't
  # the y value using strsplit. This creates a list of lists which I flatten
  # using unlist into a single list and return the first value in the list y
  unlist(strsplit(coef, lm_equation, fixed = TRUE))[1]
}
