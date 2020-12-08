# -------------------------------------------------------------------------
# Author: Sam Farmer
# Date: 12/5/2020
# assignment 6 creating your own R package
# setup -------------------------------------------------------------------

rm(list = ls())

# load devtools used to create packages
library(devtools)


# create mysummary package ------------------------------------------------
# create a new package in the specified directory
create_package("./group.regress")
