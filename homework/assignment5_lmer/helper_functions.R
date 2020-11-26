
# setup variables ---------------------------------------------------------
assignment5_data <- read.csv( "assignment5_lmer/assignment5_data.csv")
chosen_study_id <- "study_id2"
chosen_covariate <- "cov1"
chosen_random_effects <- "cov1"

# regress_group_data ------------------------------------------------------
## load packages 
pacman::p_load(dplyr, tidyr, ggplot2, data.table, broom, tidyverse, lme4)

## Problem 2
## create regress_group_data function with the following arguments
##    data = data frame
##    group_id = character vector with one or more variables to group data by
##    obs = character vector with one or more variables to summarize 
##    covs = 
##    include_intercept = 
##    ... = extra arguments for summarize_at function
regress_group_data <- function(data, group_id, obs, covs, include_intercept = TRUE, ...){
  ## produce formula for linear regression
  covariates <- paste0(covs, collapse = " + ")
  formula <- paste0(obs, ' ~ ', covariates)
  if (include_intercept == FALSE){
    formula <- paste0(formula, ' -1')
  }
  ## grouped linear regression
  df <- data %>% 
    group_by_at(.vars = group_id) %>% 
    do(tidy(lm(formula, data = ., ...))) %>%
    select(c(group_id, 'term', 'estimate')) 
  
  ## transform data to produce data frame
  df <- pivot_wider(df, id_cols = group_id, names_from = 'term', values_from = 'estimate')
  names(df)[names(df) == "(Intercept)"] <- "intercept"
  return(as.data.frame(df))
}


# problem 2 regress setup and residual calc -------------------------------
# sets up the regress_group_data call using preconfigured values of chosen_study_id
# obs and assignment5_data as data. Accepts a vector argument covs e.g. c("exposure")
# returns the group by regression on those covariates grouped by study id
regress_setup <- function(covs){
  regress_group_data(data = assignment5_data, group_id = chosen_study_id,
                     obs = "obs", covs = covs,
                     include_intercept = FALSE)
}

# calculates the residual for the passed covariate. Accepts singular covariate
# e.g. "cov1"
residual_calculator <- function(cov){
  if(length(cov) == 1){
    calculated_residual <- "assignment5_data$obs - assignment5_data$exposure *"
    if (cov != "exposure"){
      calculated_residual <- paste(calculated_residual,
                                   " coef_", cov, "[1] - assignment5_data$",
                                   cov, " * coef_", cov, "[2]", sep = "")
    } else {
      calculated_residual <- paste(calculated_residual, " coef_exposure[1]")
    }
    eval(parse(text = calculated_residual))
  } else {
    stop("Can only calcuate residual for one covariate")
  }
}

