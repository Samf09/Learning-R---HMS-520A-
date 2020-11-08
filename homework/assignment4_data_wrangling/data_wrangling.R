# -------------------------------------------------------------------------
# Author: Sam Farmer
# date: 11/7/2020
# assignment 4 data wrangling. This assignment will go over data frame manipulation
# and creating new functions that will be used for future assignments
# setup -------------------------------------------------------------------

rm(list = ls())

library(tidyr)
library(dplyr)

iris_withid <- as.data.frame(iris %>% group_by(Species) %>% 
                               mutate(group_id=rep(1:2, each=25)))
group_id_test <- c("Species", "group_id")
obs_summarise <- c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")
obs_regress <- "Sepal.Length"
covs_regress <- c("Sepal.Width", "Petal.Length")
# problem 1 ---------------------------------------------------------------
# Create a summarise_group_data function with args [data, group_id, obs, fun, ...]
# the function will return a dataframe with 'group_id' columns and summary statistics

summarise_group <- function(data, group_id, obs, fun, ...){
  grouped_data <- group_by_at(data, group_id) # group by at groups by vector
  as.data.frame(summarise_if(select(grouped_data, c(group_id, obs)),
                             # select the groups and obs cols for return
                             is.numeric,
                             fun), ... = ...)
}

mean_data <- summarise_group(iris_withid, group_id_test, obs_summarise, "mean")

# problem 2 ---------------------------------------------------------------
# Create a regress_group_data function with args [data, group_id, obs, covs, include_intecept, ...]
# the function will return a dataframe with group_id and coefficients

regress_group_data <- function(data, group_id, obs, covs,
                               include_intercept = TRUE, ...){
  grouped_data <- group_by_at(data, group_id)
  formula = "1 + "
  intercept_name <- NA
  if (include_intercept){
    formula = paste(obs, " ~", formula)
    intercept_name <- obs
  }
  for (ind_var in covs){
    formula = paste(formula, " + ", ind_var)
  }
  results <- lm(formula = as.formula(formula),
                data = grouped_data, ... = ...)
  num_col = length(covs) + ifelse(include_intercept, 1, 0)
  coefficient <- list()
  for (i in 1:num_col){
    coefficient[[i]] <- results$coefficients[i]
  }
  coefficient <- rbind(coefficient)
  colnames(coefficient) <- c(obs, covs)
  coefficient
}

lmreturn <- regress_group_data(iris_withid, group_id_test, obs_regress, covs_regress)
