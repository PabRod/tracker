# Clear environment
rm(list=ls())

# Load libraries
library(dplyr)
library(Hmisc)
library(tidyr)

# Source all functions
sapply(list.files('R', full.names = T), source)

# Make heatmaps for Gammarus
plot_heatmap(test_date = '2019-07-31', chemical = 'FLU', timebin = 5, exp_dur = 480)
plot_heatmap(test_date = '2019-07-31', chemical = 'FLU', timebin = 10, exp_dur = 480)
plot_heatmap(test_date = '2019-07-31', chemical = 'SMX', timebin = 5, exp_dur = 480)
plot_heatmap(test_date = '2019-07-31', chemical = 'SMX', timebin = 10, exp_dur = 480)

# Make heatmaps for Potamopyrgus
plot_heatmap(test_date = '2019-08-01', chemical = 'FLU', timebin = 10, exp_dur = 240)
plot_heatmap(test_date = '2019-08-01', chemical = 'SMX', timebin = 10, exp_dur = 240)
