# Clear environment
rm(list=ls())

# Load libraries
library(dplyr)
library(MASS)

# Source all functions
sapply(list.files('R', full.names = T), source)

# Create list of test dates
test_dates <- c('2019-07-31', '2019-08-01', '2019-08-08', '2019-08-15', '2019-08-15', 
                '2019-08-23', '2019-08-30', '2019-09-06', '2019-10-07', '2019-10-08')
test_species <- c('Gammarus', 'snail', 'Gammarus', 'Gammarus', 'snail', 
                  'Gammarus', 'Gammarus', 'snail', 'snail', 'Gammarus')

# Loop through test dates
for(n in c(1,4,6,10)){ #set as 1:length(test_dates) if you want to go through all data
  
  # List files
  date <- test_dates[[n]]
  species <- test_species[n]
  files <- list.files('data/raw data', full.names = T)
  files <- files[grep(date, files, value = F)]
  files <- files[grep(species, files, value = F)]
  #files <- files[grep('raw_0001.csv', files, value = F)]
  
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
    data <- lapply(data, function(x){ append_time_bins(x) }) # TODO only for Gammarus
    # Append dynamics to all locations
    data <- lapply(data, function(x){ append_dynamics(x) })
    # Add experimental data
    data <- lapply(data, function(x){ append_exp_info(x, files[[file.nr]]) })
    # Append polar coordinates and make histogram of radius distribution
    data <- lapply(data, function(x){ append_polar_coordinates(x) })
    # Collect all data together
    data <- do.call('rbind', data)
    # Store in output list
    output[[file.nr]] <- data
  }
  
  # Combine all data together
  output_data <- do.call('rbind', output)
  # Load treatment data
  if(n > 5){ # After n = 5 comes the lab data
    treatments <- read.csv('data/treatments_lab.csv')
  } else{
    treatments <- read.csv('data/treatments.csv')
  }
  # Add treatment data to rest of data
  output_data <- merge(x = output_data, y = treatments, by.x = "cosm_nr", by.y = "Cosm")
  
  # Save data
  save(output_data, file = paste('output/', species, '_', date, '.Rda', sep = ''))
}