# Clear environment
rm(list=ls())

load('output/2019-10-25.Rda')

# Load libraries
library(dplyr)
library(MASS)
library(nlme)

# Source all functions
sapply(list.files('R', full.names = T), source)

# Create list of test dates
test_dates <- c('2019-07-31', '2019-10-25')

test.date <- 2

# List files
date <- test_dates[[test.date]]
files <- list.files(paste('data/', date, sep = ''), full.names = T)
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
  data <- lapply(data, function(x){ append_time_bins(x) }) # TODO only for Gammarus
  # Append dynamics to all locations
  data <- lapply(data, function(x){ append_dynamics(x) })
  # Add experimental data
  data <- lapply(data, function(x){ append_exp_info(x, files[[file.nr]]) })
  # Append polar coordinates and make histogram of radius distribution
  #data <- lapply(data, function(x){ append_polar_coordinates(x) })
  # Collect all data together
  data <- do.call('rbind', data)
  # Store in output list
  output[[file.nr]] <- data
}

# Combine all data together
output_data <- do.call('rbind', output)

data <- output_data

# Create timebins
bins <- seq(0, 600, 10)
# Add timebins to data
data$group <- cut(data$time, bins, labels = FALSE)
# Remove NAs (time > 480)
data <- data[!is.na(data$group),]
# Add interaction between timebin and treatment group
#data$combined_group <- interaction(data$group, data$Treatment_conc)
# Add an interaction between the three factors, individual, time, and treatment
data$combined_group <- interaction(data$ind, # take ind out to average per cosm
                                   data$group) 

# Calculate average speed at all time bins over all cosms/individuals
data_summarised <- data %>% group_by(combined_group) %>%
  summarise(avaspeed = mean(aspeed),
            sdaspeed = sd(aspeed),
            group = mean(group),
            time_bin = mean(time_bin),
            ind = mean(ind))
# Lights off as 0, lights on as 1
data_summarised$light_on_off <- ifelse(data_summarised$time_bin == 1 | 
                                         data_summarised$time_bin == 3, 0, 1)
# Fit lme model
ctrl <- lmeControl(opt='optim')
model_1 <- lme(avaspeed ~ light_on_off, 
               random = ~ 1+ group | ind,
               data = data_summarised, control=ctrl)
anova(model_1)
summary(model_1)

# Calculate average speed at all time bins over all cosms/individuals
data_summarised <- data %>% group_by(group) %>%
  summarise(avaspeed = mean(aspeed),
            sdaspeed = sd(aspeed))

# Make plot (Super large variation, so turn errorbars on or off)
p <- ggplot(data_summarised, aes(x=group, y=avaspeed)) + 
  geom_line() +
  geom_point() 
  geom_errorbar(aes(ymin=avaspeed-sdaspeed, ymax=avaspeed+sdaspeed), width=.2,
                position=position_dodge(0.05))
p
