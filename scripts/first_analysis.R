# Clear environment
rm(list=ls())

# Load libraries
library(dplyr)
library(MASS)

# Source all functions
sapply(list.files('R', full.names = T), source)

# List files
files <- list.files('data/2019-07-31', full.names = T)
files <- files[grep('raw_0001.csv', files, value = F)]

# Create empty list to collect output data
output <- list(0)

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
  # Append polar coordinates and make histogram of radius distribution
  data <- lapply(data, function(x){ append_polar_coordinates(x) })
  # Add cosm nr to data
  data <- lapply(data, function(x){ append_exp_info(x, files[[file.nr]]) })
  # Collect all data together
  data <- do.call('rbind', data)
  # Store in output list
  output[[file.nr]] <- data
}

# Combine all data together
output_data <- do.call('rbind', output)

# Load treatment data
treatments <- read.csv('data/treatments.csv')
# Add treatment data to rest of data
output_data <- merge(x = output_data, y = treatments, by.x = "cosm_nr", by.y = "Cosm")
