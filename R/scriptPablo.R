# data with individuals from one cosm is my starting point
#load('output/Gammarus_2019-08-15.Rda')
# data <- output_data

# Load required libraries
#library(dplyr)
#library(ggplot2)
#library(magrittr)

# 
data %>%
  mutate(Treatment_conc = factor(Treatment_conc, 
                                 levels = c(0, 0.1, 1, 10, 100))) %>%
  ggplot(aes(x = r, group = Treatment_conc, color = Treatment_conc)) + geom_density() + xlim(0, 50)
  
data %>%
  mutate(Treatment_conc = factor(Treatment_conc, 
                                 levels = c(0, 0.1, 1, 10, 100))) %>%
  ggplot(aes(x = aaccel, group = light_on_off, color = light_on_off)) + geom_density() + xlim(0, 1000)

data %>%
  mutate(Treatment_conc = factor(Treatment_conc, levels = c(0, 0.1, 1, 10, 100))) %>%
  ggplot(aes(x = aaccel, group = cosm_nr, color = Treatment_conc)) + geom_density() + xlim(0, 1000)

data %>%
  mutate(Treatment_conc = factor(Treatment_conc, 
                                 levels = c(0, 0.1, 1, 10, 100))) %>%
  ggplot(aes(x = time, y = aaccel, group = Treatment_conc, color = Treatment_conc)) + geom_line() + ylim(0, 1000)
