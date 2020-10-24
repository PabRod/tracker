# Clear environment
rm(list=ls())

## Load packages
library(dplyr)

## Preprocess data
#source('scripts/data_assembly.R')


######### Load all pre-processed data and combine all gammarus experiments together

## Cosm data
load('output/Gammarus_2019-08-15.Rda')
gammarus_acute_cosm <- output_data
# Add test location
gammarus_acute_cosm$test_location <- 'field'
# Add test (exposure) duration
gammarus_acute_cosm$test_duration <- 2
load('output/Gammarus_2019-07-31.Rda')
gammarus_chronic_cosm <- output_data
gammarus_chronic_cosm$test_location <- 'field'
gammarus_chronic_cosm$test_duration <- 21
## Lab data
load('output/Gammarus_2019-08-23.Rda')
gammarus_acute_lab <- output_data
# Remove some irrelevant columns
gammarus_acute_lab <- subset(gammarus_acute_lab, select = -c(species, feeding, test_duration))
gammarus_acute_lab$test_location <- 'lab'
gammarus_acute_lab$test_duration <- 2
load('output/Gammarus_2019-10-08.Rda')
gammarus_chronic_lab <- output_data
# Remove some irrelevant columns
gammarus_chronic_lab <- subset(gammarus_chronic_lab, select = -c(species, feeding, test_duration))
gammarus_chronic_lab$test_location <- 'lab'
gammarus_chronic_lab$test_duration <- 21

## Combine all data together
gammarus_data <- do.call('rbind', list(gammarus_acute_cosm, gammarus_chronic_cosm,
                                       gammarus_acute_lab, gammarus_chronic_lab))
## Remove all other objects to reduce memory usage
rm(list = c('gammarus_acute_cosm', 'gammarus_chronic_cosm',
   'gammarus_acute_lab', 'gammarus_chronic_lab', 'output_data'))


######### Here starts an overview of the data, and the columns which are important for our analysis

## See data
head(gammarus_data)
## Swimming velocity is in the aspeed column
head(gammarus_data$aspeed)
## We are only interested in Fluoxetine data, so remove SMX data (other chemical we also tested)
gammarus_data <- filter(gammarus_data, Treatment_chem != 'SMX')
# The field data only used a solvent control, whilst the lab data had both a solvent and a normal control
# A check whether the solvent control is significantly different from the normal control is therefore necessary
unique(gammarus_data$Treatment_chem) 
## The Treatment_conc column contains the different concentrations tested
unique(gammarus_data$Treatment_conc) 
## The four different light intervals (off-on-off-on) are in the light_interval column
unique(gammarus_data$light_interval)
## Binary variable indicating when light is on or off
unique(gammarus_data$light_on_off)
## Create 10 second timebins for the 8 minutes (480 seconds) duration of measurement
bins <- seq(0, 480, 10)
# Add timebins to data
gammarus_data$bins <- cut(gammarus_data$time, bins, labels = FALSE)
# Remove NAs (time > 480)
gammarus_data <- gammarus_data[!is.na(gammarus_data$bins),]
unique(gammarus_data$bins) 

## Remove errors
unrealistic_speed <- mean(gammarus_data$aspeed)+2*sd(gammarus_data$aspeed)
gammarus_data <- filter(gammarus_data, aspeed < unrealistic_speed)

## Change variables into factors
gammarus_data <- gammarus_data %>% 
  dplyr::mutate(laspeed = log(aaccel), 
                Treatment_conc = factor(Treatment_conc, 
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
## Summarise to 10 second bins
gammarus_data <- gammarus_data %>% group_by(
  cosm_nr, ind, Treatment_chem, Treatment_conc, test_location, bins,
  test_duration, light_interval, light_on_off) %>%
  summarise(
    avlaspeed = mean(laspeed),
    vavlaspeed = var(laspeed),
    number = n()) %>%
  mutate(selaspeed = vavlaspeed/number)


## Save final combined data
save(gammarus_data, file = 'output/gammarus_data_final.Rda') # This is the object Daniel needs
