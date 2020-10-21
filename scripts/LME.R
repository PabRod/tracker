# Clear environment
rm(list=ls())

## Start here
library(nlme)
library(dplyr)
library(lattice)


# Now try with own data
load('output/2019-08-08.Rda')
prepare_lme <- function(input_data, chemical, exp_dur, timebin){
  # Only focus on one chemical
  data <- dplyr::filter(input_data, Treatment_chem == chemical | Treatment_chem == 'C')
  # Create timebins
  bins <- seq(0, exp_dur, timebin)
  # Add timebins to data
  data$group <- cut(data$time, bins, labels = FALSE)
  # Remove NAs (time > 480)
  data <- data[!is.na(data$group),]
  # Look only at two treatments
  #data <- filter(data, Treatment_conc == 100 | Treatment_conc == 0.1)
  # Add an interaction between the three factors, individual, time, and treatment
  data$combined_group <- interaction(data$ind, # take ind out to average per cosm
                                     data$group, 
                                     data$cosm_nr,
                                     data$test_duration) 
  # Summarise on these groups
  data_summarised <- data %>% group_by(combined_group) %>%
    summarise(avaspeed = mean(aspeed),
              cosm_nr = mean(cosm_nr),
              timebin = mean(group),
              ind = mean(ind),
              Treatment_conc = mean(Treatment_conc),
              light_on_off = mean(time_bin),
              expsr_dur = test_duration[[1]])
  # Lights off as 0, lights on as 1
  #data_summarised$light_on_off <- ifelse(data_summarised$light_on_off == 1 | 
  #                                         data_summarised$light_on_off == 3, 0, 1)
  data_summarised$Treatment_conc <- factor(data_summarised$Treatment_conc)
  data_summarised$expsr_dur <- factor(data_summarised$expsr_dur)
  #data_summarised$timebin <- factor(data_summarised$timebin)
  
  return(data_summarised)
}


# Fit lme model
ctrl <- lmeControl(opt='optim')
model_1 <- lme(mean.speed ~ timebin*Treatment_conc + timebin*light_on_off +
                 Treatment_conc*light_on_off, 
               random = ~ 1+ timebin | cosm_nr/ind,
               data = data_summarised, control=ctrl)
# Reduce model by removing unimportant interaction
model_1 <- lme(mean.speed ~ timebin*light_on_off +
                 Treatment_conc*light_on_off, 
               random = ~ 1+ timebin | cosm_nr/ind,
               data = data_summarised, control=ctrl)

# And see output
summary(model_1)
anova(model_1)
glht(model_1, linfct=mcp(Treatment_conc="Tukey"))
summary(glht(model_1, linfct=mcp(Treatment_conc="Tukey")))


# Convert Treatment_conc to factor
data_summarised$Treatment_conc <- as.factor(data_summarised$Treatment_conc)
# So that this can be used as a title in the strip labels later
data_summarised$Treatment_conc <- paste(data_summarised$Treatment_conc, 'ug/L')

# Make a combined group combining both location and cosm nr
data_summarised$combined_group <- interaction(data_summarised$ind, 
                                              data_summarised$cosm_nr)

# So that this data can be grouped on this combined grouping
my_results <- groupedData(mean.speed~timebin|cosm_nr, outer = ~ Treatment_conc, 
                          data = data_summarised)

# And plot
plot(my_results, outer = T, key = FALSE, aspect = 1, layout = c(5,1,1))





# For each treatment, plot the individuals per cosm
load('output/2019-07-31.Rda')
# Only focus on one chemical
data <- filter(output_data, Treatment_chem == 'FLU' | Treatment_chem == 'C')
# Create timebins
bins <- seq(0, 480, 30)
# Add timebins to data
data$group <- cut(data$time, bins, labels = FALSE)
# Remove NAs (time > 480)
data <- data[!is.na(data$group),]
# Add an interaction between the three factors, individual, time, and treatment
data$combined_group <- interaction(data$ind, data$group, data$cosm_nr)
# Summarise on these groups
data_summarised <- data %>% group_by(combined_group) %>%
  summarise(mean.speed = mean(aspeed),
            cosm_nr = mean(cosm_nr),
            timebin = mean(group),
            ind = mean(ind),
            Treatment_conc = mean(Treatment_conc))
# Make a combined group combining both location and cosm nr
data_summarised$combined_group <- interaction(data_summarised$ind, 
                                              data_summarised$cosm_nr)


# Fit linear mixed effect model
model_1 <- lme(mean.speed ~ timebin*Treatment_conc, 
               random = ~ timebin | cosm_nr/ind,
               data = data_summarised)
summary(model_1)

# Convert cosm nr to factor
data_summarised$cosm_nr <- as.factor(data_summarised$cosm_nr)
# So that this can be used as a title in the strip labels later
data_summarised$cosm_nr <- paste('cosm', data_summarised$cosm_nr, ',',
                                 data_summarised$Treatment_conc, 'ug/L')

# Group data on combination of location and animal
my_results <- groupedData(mean.speed~timebin|combined_group, outer = ~ cosm_nr, 
                          data = data_summarised)
# And plot
plot(my_results, outer = T, key = FALSE,  #layout(column,row,page)
     aspect = 1 )
