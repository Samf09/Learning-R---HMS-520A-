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

