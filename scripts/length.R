# Clear environment
rm(list=ls())

# Load libraries
library(dplyr)
library(xlsx)

length_chronic <- read.xlsx('data/Cosm Gammarus size.xlsx', 1)
# Load lengths of acute toxicity tests
length_acute <- read.xlsx('data/Cosm Gammarus size.xlsx', 2)
length_acute$ind <- as.character(length_acute$Cosm)
length_acute$ind <- gsub("\\d", "", length_acute$ind)
# Remove strange A's
length_acute$ind <- gsub("[^a-zA-Z0-9]", "", length_acute$ind)
length_acute$Cosm <- gsub("[^0-9]", "", length_acute$Cosm)

# Load velocity data
load('output/2019-07-31.Rda')
# Only focus on controls
data <- filter(output_data, Treatment_conc == 0)
alphabet <- c('A', 'B', 'C', 'D', 'E', 'F', 'H', 'I', 'J')
data$ind <- alphabet[data$ind]
data2 <- merge(data, length_acute, by.x = c('cosm_nr', 'ind'), by.y = c('Cosm', 'ind'))
# Only look at the first 2 minutes
data2 <- filter(data2, time_bin == 1)

# Create timebins
bins <- seq(0, 120, 30)
# Add timebins to data
data2$group <- cut(data2$time, bins, labels = FALSE)
# Create interaction between cosm-ind-timebin
data2$combined_group <- interaction(data2$cosm_nr, data2$ind)
# Correct velocity with length
data2$vel.cor <- data2$aspeed/data2$Lenght

# Summarise to average velocity per individual
data2_summarised <- data2 %>% group_by(combined_group) %>%
  summarise(mean.vel = mean(vel.cor), # Using the velocity corrected by length
            ind = ind[1],
            cosm = mean(cosm_nr),
            length = mean(Lenght))
data2_summarised <- data2 %>% group_by(combined_group) %>%
  summarise(mean.vel = mean(aspeed), # Or using the non-standardized velocity
            ind = ind[1],
            cosm = mean(cosm_nr),
            length = mean(Lenght))
#data2 <- filter(data2, aspeed < 50)
# Filter out NAs (missing lengths)
data2_summarised <- data2_summarised[!is.na(data2_summarised$length),]
#data2_summarised <- data2_summarised[-c(31),]
plot(x = data2_summarised$cosm, y = data2_summarised$mean.vel)
