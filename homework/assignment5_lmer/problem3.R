# -------------------------------------------------------------------------
# Assignment 5 linear mixed effect modeling
# date: 11/21/2020
# author: Sam Farmer
# setup -------------------------------------------------------------------

rm(list = ls())

setwd("repos/HMS520A_Autumn2020_Sam_Farmer/homework/")
# load in the custom function regress_group_data, regress_setup, and residual_calculator
# along with necessary packages dplyr, tidyr, ggplot2, data.table, broom, tidyverse
source("./assignment5_lmer/helper_functions.R")


# problem 3 ---------------------------------------------------------------
#' After we have selected the study id and covariates, we need to determine 
#' which variable should differ from study to study (random effects).
#' Use the regress_group_data function again but include exposure and the
#' covariate you selected to compute the coefficients across the studies.

coef_cov1 <- regress_setup(c("exposure", "cov1"))
coef_cov1
# > coef_cov1
# study_id2     exposure       cov1
# 1          1 0.7022422 1.66096595
# 2          2 1.5951737 0.20300116
# 3          3 0.8027432 2.30969405
# 4          4 0.8410838 1.20965388
# 5          5 0.8392084 1.72253566
# 6          6 1.0682848 1.81005574
# 7          7 1.0516659 0.06043489
# 8          8 0.3802624 2.20263082
# 9          9 0.3539708 2.37274530
# 10        10 0.5444109 1.13573447

var(coef_cov1$exposure)
# > var(coef_cov1$exposure)
# [1] 0.1352939

var(coef_cov1$cov1)
# > var(coef_cov1$cov1)
# [1] 0.6715912

# Variance is higher in cov1 across study_id so it will be used for random
# effects