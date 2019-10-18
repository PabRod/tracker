# Clear environment
rm(list=ls())

# Load libraries
library(dplyr)
library(Hmisc)
library(tidyr)

# Load input data
load('output/2019-07-31.Rda')

# Only focus on FLU data
data <- filter(output_data, Treatment_chem == 'FLU' | Treatment_chem == 'C')

# Create bins/ 10 second groups
bins <- seq(0, 480, 5) # 480 for 8 min
# Add groups to data
data$group <- cut(data$time, bins, labels = FALSE)
# Add interaction between timebin and treatment group
data$combined_group <- interaction(data$group, data$Treatment_conc)

# Summarise by group
summarised_data <- data %>% group_by(combined_group) %>%
  summarise(mean.speed = mean(aspeed),
            group = mean(group),
            Treatment_conc = mean(Treatment_conc))
# Remove strange row
summarised_data <- summarised_data[-481,]  # TODO check how this row is made

# Convert into wide format, using 10s timebins as columns
summarised_data <- spread(summarised_data[2:4], group, mean.speed)

# Plot heatmap
heatmap(as.matrix(summarised_data[-1]), Colv = NA, Rowv = NA, scale = 'row',
        col = rev(heat.colors(256)))
