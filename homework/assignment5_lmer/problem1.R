# -------------------------------------------------------------------------
# Assignment 5 linear mixed effect modeling
# date: 11/21/2020
# author: Sam Farmer
# -------------------------------------------------------------------------
rm(list = ls())

library(tidyverse)

repo_path <- "repos/HMS520A_Autumn2020_Sam_Farmer/homework/"

data <- read.csv("assignment5_lmer/assignment5_data.csv")
# problem 1 ---------------------------------------------------------------
# plot obs against exposure by study_id1 and study_id2. Which one do you think 
# is better to use as a grouping id? Explain your reason.
# study_id1 plots
ggplot(subset(data, select = c(exposure, obs, study_id1)), 
       aes(x = obs, y = exposure)) + geom_line(aes(color = factor(study_id1))) + 
  ggtitle("Study Id 1") + facet_wrap(~study_id1, nrow=2)

# study_id2 plots
ggplot(subset(data, select = c(exposure, obs, study_id2)),
       aes(x = obs, y = exposure)) +
  geom_line(aes(color = factor(study_id2))) + 
  ggtitle("Study Id 2") + facet_wrap(~study_id2, nrow=2)

# both study ids are pretty messy. Study id 2 looks to be less of a jumble
# comparatively to study id 1 when exposure is plotted as y and obs as x. Its 
# lines look closer to linear and I would assume easier to model off of and appear
# to cover more of the data for obs. Less lines are cut off early or start later

