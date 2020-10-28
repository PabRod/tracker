# Clear environment
rm(list=ls())

# Load libraries
library(dplyr)
library(MASS)

# Source all functions
sapply(list.files('R', full.names = T), source)

assemble_data <- function(date, species, save = TRUE) {
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
    # Append light interval
    data <- lapply(data, function(x){ append_light_interval(x) }) # TODO only for Gammarus
    ## Convert the unit of time from microseconds to seconds
    data <- lapply(data, function(x){x$time <- x$time/1e6; x})
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
  if(date %in% c('2019-07-31', '2019-08-15')){ # These dates are the lab exps
    treatments <- read.csv('data/treatments_lab.csv')
    treatments$test_location <- 'lab' # Add test_location
  } else{
    treatments <- read.csv('data/treatments.csv')
    treatments$test_location <- 'field'
  }
  # Add treatment data to rest of data
  output_data <- merge(x = output_data, y = treatments, by.x = "cosm_nr", by.y = "Cosm")
  
  # Save data
  if(save) {
    save(output_data, file = paste('output/', species, '_', date, '.Rda', sep = ''))
  }
  
  return(output_data)
}

## Create list of test dates
#test_dates <- c('2019-07-31', '2019-08-01', '2019-08-08', '2019-08-15', '2019-08-15', 
#                '2019-08-23', '2019-08-30', '2019-09-06', '2019-10-07', '2019-10-08')
#test_species <- c('Gammarus', 'snail', 'Gammarus', 'Gammarus', 'snail', 
#                  'Gammarus', 'Gammarus', 'snail', 'snail', 'Gammarus')

# Apply function to experiments of interest
test_dates <- c('2019-07-31','2019-08-15','2019-08-23','2019-10-08')
output_data <- lapply(test_dates, function(x){ assemble_data(date = x, species = 'Gammarus') })

## Combine experiments of interest together
gammarus_data <- do.call('rbind', output_data)

## We are only interested in Fluoxetine data, so remove SMX data (other chemical we also tested)
gammarus_data <- filter(gammarus_data, Treatment_chem != 'SMX')

## Change variables into factors
gammarus_data <- gammarus_data %>% 
  dplyr::mutate(Treatment_conc = factor(Treatment_conc, 
                                        levels = c(0, 0.1, 1, 10, 100)),
                Treatment_chem = factor(Treatment_chem, 
                                        levels = c("C", "SC", "FLU")),
                test_duration = factor(test_duration, 
                                       levels = c("21","2")),
                test_location = factor(test_location, 
                                       levels = c("field","lab")),
                light_interval = factor(light_interval, 
                                        levels = 1:4, 
                                        labels = c("off1", "on1", "off2", "on2")),
                light_on_off = factor(light_on_off, levels = c(0, 1),
                                      labels = c("off", "on")))