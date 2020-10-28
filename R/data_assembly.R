#' Assemble all measurements, files, and experiments
#'
#' @param date The date of the experiment
#' @param species The name of the test species
#' @param save Dummy variable indicating intermittent saving
#'
#' @return All the data for one experiment
#' @export
#'
#'

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
    # Collect all locations together
    data <- do.call('rbind', data)
    # Store in output list
    output[[file.nr]] <- data
  }
  
  # Combine all files together
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