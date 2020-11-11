# -------------------------------------------------------------------------
# Author: Sam Farmer
# date: 11/7/2020
# assignment 4 data wrangling. This assignment will go over data frame manipulation
# and creating new functions that will be used for future assignments
# setup -------------------------------------------------------------------

rm(list = ls())

library(tidyverse)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plyr)

# Example data. Iris data with added group_id column
iris_withid <- as.data.frame(iris %>% group_by(Species) %>% 
                               dplyr::mutate(group_id=rep(1:2, each=25)))
group_id_iris <- c("Species", "group_id")
obs_iris <- "Sepal.Length"
covs_iris <- c("Sepal.Width", "Petal.Length")

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

mean_data <- summarise_group(iris_withid, group_id_iris, all_of(obs_iris), "mean")

# problem 2 ---------------------------------------------------------------
# Create a regress_group_data function with args [data, group_id, obs, covs, include_intecept, ...]
# the function will return a dataframe with group_id and coefficients

regress_group_data <- function(data, group_id, obs, covs,
                               include_intercept = TRUE, ...){
  # Create formula in the form obs ~ 1 + covs[1] + ... +covs[n]
  formula = "1 + "
  if (include_intercept){
    formula = paste(obs, " ~ ", formula)
  }
  for (ind_var in covs){
    formula = paste(formula, " + ", ind_var)
  }
  
  # for each subset of data apply linmod then combine results into a list
  # makes list of lm calls and results
  # e.g. using iris data grouped by Species and group_id 
  # formula = Sepal.Length ~ 1 + Sepal.Width + Petal.Length:
  # $setosa.1
  # 
  # Call:
  #   lm(formula = as.formula(formula), data = df)
  # 
  # Coefficients:
  #   (Intercept)   Sepal.Width  Petal.Length  
  # 1.3792        0.8473        0.4797  
  # 
  linmod <- function(df){
    lm(formula = as.formula(formula), data = df, ... = ...)
    }
  models <- dlply(data, group_id, linmod)
  
  # for each model in models list apply coefficient then combine into a data.frame
  lm_dataframe <- ldply(models, coef)
}

lmreturn <- regress_group_data(iris_withid, group_id_iris, obs_iris, covs_iris)

# problem 3 ---------------------------------------------------------------
# Clean multiple datasets following tidyr principals 
# https://r4ds.had.co.nz/tidy-data.html#pivoting
# Load in who TB data and clean it
who <- tidyr::who
df_tb <- who %>% pivot_longer(cols = new_sp_m014:newrel_f65,
                              names_to = "key",
                              values_to = "cases",
                              values_drop_na = TRUE
                              ) %>%
                 mutate(key = stringr::str_replace(key, "newrel", "new_rel")
                         ) %>%
                 separate(key, c("new", "var", "sexage")) %>%
                 select(-new, -iso2, -iso3) %>%
                 separate(sexage, c("sex", "age"), sep = 1)

df_pop_path <- paste("/Users/sam/repos/HMS520A_Autumn2020_Sam_Farmer/homework",
                     "/assignment4_data_wrangling/API_SP.POP.TOTL_DS2_en_csv_",
                     "v2_1637443/API_SP.POP.TOTL_DS2_en_csv_v2_1637443.csv",
                     sep = "")
df_pop <- read_csv(df_pop_path, skip = 4)
df_pop <- df_pop %>% pivot_longer(cols = `1960`:`2020`,
                                  names_to = "year",
                                  values_to = "pop",
                                  values_drop_na = TRUE) %>% 
                    select(-`Country Code`, -`Indicator Name`,
                           -`Indicator Code`, -X66)
#
df_pop$year <- as.integer(df_pop$year)
names(df_pop)[names(df_pop) == "Country Name"] <- "country"
df_pop_tb <- inner_join(df_tb, df_pop, by=c("country", "year"))
df_pop_tb <- df_pop_tb %>% mutate(incidence_ratio = cases / pop)

country_regression <- regress_group_data(df_pop_tb, "country",
                                         "incidence_ratio", "year")

# make plots!
# sort by year
country_regression_sorted <- country_regression[order(country_regression$year), ]

# make country a factor for plots
country_regression_sorted$country <- factor(country_regression_sorted$country,
                                     levels = country_regression_sorted$country)
# plot
ggplot(data = country_regression_sorted) + 
  geom_bar(aes(x = country, y = year), stat = "identity") +
  labs(title = "Year coefficient ordered bar chart") +
  theme(axis.text.x = element_text(angle = 65, vjust=0.6))

