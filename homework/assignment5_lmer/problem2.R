# -------------------------------------------------------------------------
# Assignment 5 linear mixed effect modeling
# date: 11/21/2020
# author: Sam Farmer
# setup -------------------------------------------------------------------

rm(list = ls())

# setwd("repos/HMS520A_Autumn2020_Sam_Farmer/homework/")
library(tidyverse)
source("assignment4_data_wrangling/regress_group_data.R")

assignment5_data <- read.csv( "assignment5_lmer/assignment5_data.csv")

chosen_study_id <- "study_id2"

# problem 2 ---------------------------------------------------------------
# In this answer we will find out which covariates should be included in the model
#1 use the 'regress_group_data' function you created in the last assignment to
# compute the coefficient for exposure of each group without intercepts

regress_setup <- function(covs){
  regress_group_data(data = assignment5_data, group_id = chosen_study_id,
                     obs = "obs", covs = covs,
                     include_intercept = FALSE)
}

residual_calculator <- function(cov){
  calculated_residual <- "assignment5_data$obs - assignment5_data$exposure *"
  if (cov != "exposure"){
    calculated_residual <- paste(calculated_residual,
                                 " coef_", cov, "[1] - assignment5_data$",
                                 cov, " * coef_", cov, "[2]", sep = "")
  } else {
    calculated_residual <- paste(calculated_residual, " coef_exposure[1]")
  }
  eval(parse(text = calculated_residual))
}

# calculate coefficient of just exposure
coef_exposure <- regress_setup("exposure")
residual_exposure <- residual_calculator("exposure")

# calculate coefficient of exposure and cov1
coef_cov1 <- regress_setup(c("exposure", "cov1"))
residual_cov1 <- residual_calculator("cov1")

# calculate coefficient of exposure and cov2  
coef_cov2 <- regress_setup(c("exposure", "cov2"))
residual_cov2 <- residual_calculator("cov2")

# calculate coefficient of exposure and cov3
coef_cov3 <- regress_setup(c("exposure", "cov3"))
residual_cov3 <- residual_calculator("cov3")


# analyze results! --------------------------------------------------------
#' To effectively chose which covariates to keep in the model we need to see
#' which ones have lowered the residual rather than not impacted it at all
#' or made it increase

data.frame(residual_exposure = residual_exposure,
           residual_cov1 = residual_cov1,
           residual_cov2 = residual_cov2,
           residual_cov3 = residual_cov3)
#     exposure     cov1            cov2          cov3
# 1   0.2987110  0.005421838   0.2987110   0.2987110
# 2   0.1089874 -0.099494898   0.1089874   0.1089874
# 3   0.1401986 -0.025593099   0.1401986  -0.1806337
# 4  -0.2243335 -0.382197441  -0.2243335  -0.2243335
# 5  -2.0579765 -2.057976532  -3.4517345  -2.0579765
# 6  -3.4339568 -3.433956752  -5.3243303  -4.1996088
# 7  -4.6194510 -4.619450962  -5.5229100  -4.6194510
# 8  -6.5979947 -6.597994653  -8.9046503  -6.9341246
# 9  -3.8702795 -3.870279531  -5.4903361  -4.3103428
# 10 -7.8059433 -7.805943271  -9.1669691  -7.8059433

# we can see that only the 2nd column (cov1) lowers the residual especially in study_id1
# the other covariates column 3 and 4 have no impact or increase the residual