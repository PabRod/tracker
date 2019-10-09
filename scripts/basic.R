# Load libraries
library(dplyr)
library(MASS)

## Source all functions
sapply(list.files('R', full.names = T), source)

# Import data
data <- import('./data/data.csv')
data <- filter(data, location == 'Loc01')
data <- append_dynamics(data)

# Plot results
plot_positions(data, 'scatter')

plot_positions(data, 'density')
