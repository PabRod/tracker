## Clear environment
rm(list=ls())

# Load libraries
library(dplyr)
library(MASS)

## Source all functions
sapply(list.files('R', full.names = T), source)

# List files
files <- list.files('data/2019-07-31', full.names = T)
files <- files[grep('raw_0001.csv', files, value = F)]

# Loop through all files
for(file.nr in 1:length(files)){
  # Import data
  data <- import(files[[file.nr]])
  # Apply next steps to all locations, so make list of locations
  list_locations <- as.list(unique(data$location))
  # Filter on location
  data <- lapply(list_locations, function(x){ filter_data(data, filter_location = x) })
  # Append timebins
  data <- lapply(data, function(x){ append_time_bins(x) })
  # Append dynamics to all locations
  data <- lapply(data, function(x){ append_dynamics(x) })
  ## Append polar coordinates and make histogram of radius distribution
  data <- lapply(data, function(x){ append_polar_coordinates(x) })
  ## Add cosm nr to data
  data <- lapply(data, function(x){ append_cosm_nr(x, files[[file.nr]]) })
}
