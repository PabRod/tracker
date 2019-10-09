# Load libraries
library(dplyr)
library(MASS)

# Import data
data <- import('./data/data.csv')
data <- filter(data, location == 'Loc01')
data <- append_dynamics(data)

# Plot results
plot_positions(data, 'scatter')

plot_positions(data, 'density')
