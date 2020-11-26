# -------------------------------------------------------------------------
# Assignment 5 linear mixed effect modeling
# date: 11/25/2020
# author: Sam Farmer
# setup -------------------------------------------------------------------

rm(list = ls())
setwd("repos/HMS520A_Autumn2020_Sam_Farmer/homework/")
# load in assignment5_data
# load in the custom function regress_group_data, regress_setup, and residual_calculator
# along with necessary packages dplyr, tidyr, ggplot2, data.table, broom, tidyverse, lme4
source("./assignment5_lmer/helper_functions.R")


# problem 4 ---------------------------------------------------------------
formula_prob4 <- paste("obs ~ exposure + ", chosen_covariate, " + (",
                       chosen_random_effects, " || ", chosen_study_id,")", sep="")

fit <- lmer(as.formula(formula_prob4), data = assignment5_data)

study_id2_preds <- predict(fit, newdata = assignment5_data)
data_with_preds <- cbind(assignment5_data, lmer_preds = study_id2_preds)

# plots! ------------------------------------------------------------------
ggplot(data = data_with_preds, aes(x=obs, y=exposure)) +
  geom_line(aes(color = factor(study_id2))) +
  facet_wrap(~study_id2, nrow=2) +
  geom_line(aes(x=obs, y=lmer_preds))
