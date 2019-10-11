# Load libraries
library(dplyr)
library(MASS)

## Source all functions
sapply(list.files('R', full.names = T), source)

# Import data
data <- import('./data/data.csv')

# Apply next steps to all locations
list_locations <- as.list(unique(data$location))
# Filter on location
data <- lapply(list_locations, function(x){ filter_data(data, filter_location = x) })
# Append dynamics to all locations
data <- lapply(data, function(x){ append_dynamics(x) })
## Append polar coordinates and make histogram of radius distribution
data <- lapply(data, function(x){ append_polar_coordinates(x) })

# Plot results
lapply(data, function(x) {plot_positions(x, 'scatter')})
lapply(data, function(x) {plot_positions(x, 'density')})
lapply(data, function(x) {hist(x$r, breaks = 50, freq = FALSE)})
lapply(data, function(x) {hist(x$th, breaks = 50, freq = FALSE)})
