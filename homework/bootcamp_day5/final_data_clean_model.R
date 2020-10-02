rm(list = ls())

# setup -------------------------------------------------------------------

library(tidyverse)
library(modelr)
library(readr)
library(ggplot2)


# load data ---------------------------------------------------------------

final_data <- read_csv("repos/BootCamp/data/final_data.csv")

clean_data <- replace_na(final_data, list(y=0, y_se=0))

group_split <- group_split(clean_data, group_id)

ggplot(clean_data, aes(x, y)) + geom_point()
# model -------------------------------------------------------------------
# slope, intercept, 

linear_model <- function(data){
  return(lm(y ~ x, data = data))
}
measure_distance <- function(mod, data) {
  diff <- data$y - model1(mod, data)
  sqrt(mean(diff ^ 2))
}

group1_mod <- optim(c(0, 0), measure_distance, data = group_split[1][[1]])
group2_mod <- optim(c(0, 0), measure_distance, data = group_split[2][[1]])
group3_mod <- optim(c(0, 0), measure_distance, data = group_split[3][[1]])
group4_mod <- optim(c(0, 0), measure_distance, data = group_split[4][[1]])
group5_mod <- optim(c(0, 0), measure_distance, data = group_split[5][[1]])

ggplot(clean_data, aes(x, y)) + 
  geom_point(size = 2, colour = "grey30") + 
  geom_abline(intercept = group1_mod$par[1], slope = group1_mod$par[2]) +
  geom_abline(intercept = group2_mod$par[1], slope = group2_mod$par[2]) +
  #geom_abline(intercept = group3_mod$par[1], slope = group3_mod$par[2]) +
  geom_abline(intercept = group4_mod$par[1], slope = group4_mod$par[2]) +
  geom_abline(intercept = group5_mod$par[1], slope = group5_mod$par[2])
