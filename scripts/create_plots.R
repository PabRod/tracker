# Clear environment
rm(list=ls())

# Load libraries
library(dplyr)
library(Hmisc)
library(tidyr)
library(ggplot2)
library(viridis)
library(ggExtra)
library(nlme)

# Source all functions
sapply(list.files('R', full.names = T), source)

# # Make heatmaps for Gammarus chronic
# plot_heatmap(test_date = '2019-07-31', chemical = 'FLU', timebin = 10, exp_dur = 480)
# plot_heatmap(test_date = '2019-07-31', chemical = 'SMX', timebin = 10, exp_dur = 480)
# 
# # Make heatmaps for Gammarus acute
# plot_heatmap(test_date = '2019-08-08', chemical = 'FLU', timebin = 10, exp_dur = 480)
# plot_heatmap(test_date = '2019-08-08', chemical = 'SMX', timebin = 10, exp_dur = 480)
# 
# # Plot velocity for Gammarus acute
# plot_velocity(test_date = '2019-08-08', chemical = 'SMX', timebin = 10, exp_dur = 480)
# plot_velocity(test_date = '2019-07-31', chemical = 'SMX', timebin = 10, exp_dur = 480)


################### Gammarus - Cosm ###################

# Load acute and chronic tests of cosm study with Gammarus 
#load('output/Gammarus_2019-08-08.Rda')
load('output/Gammarus_2019-08-15.Rda')
gammarus_acute <- output_data
load('output/Gammarus_2019-07-31.Rda')
gammarus_chronic <- output_data

# Summarise data for FLU
gammarus_acute_FLU <- summarise_data(input_data = gammarus_acute, chemical = 'FLU',
                                 exp_dur = 480, timebin = 10, expsr_dur = '2d')
gammarus_chronic_FLU <- summarise_data(input_data = gammarus_chronic, chemical = 'FLU', 
                                   exp_dur = 480, timebin = 10, expsr_dur = '21d')
# And make a combined heatmap of the acute and chronic tests
plot_heatmap2(input_acute = gammarus_acute_FLU, 
              input_chronic = gammarus_chronic_FLU,
              species = 'Gammarus',
              chemical = 'FLU',
              exp_dur = 480,
              timebin = 10,
              test_location = 'Cosm')
# Plot velocity
plot_velocity(input_acute = gammarus_acute_FLU,
              input_chronic = gammarus_chronic_FLU,
              species = 'Gammarus',
              chemical = 'FLU',
              exp_dur = 480,
              timebin = 10,
              test_location = 'Cosm')

# Summarise data for SMX
gammarus_acute_SMX <- summarise_data(input_data = gammarus_acute, chemical = 'SMX',
                                 exp_dur = 480, timebin = 10, expsr_dur = '2d')
gammarus_chronic_SMX <- summarise_data(input_data = gammarus_chronic, chemical = 'SMX', 
                                   exp_dur = 480, timebin = 10, expsr_dur = '21d')
# And make a combined heatmap of the acute and chronic tests
plot_heatmap2(input_acute = gammarus_acute_SMX, 
              input_chronic = gammarus_chronic_SMX,
              species = 'Gammarus',
              chemical = 'SMX',
              exp_dur = 480,
              timebin = 10,
              test_location = 'Cosm')

################### Potamopyrgus - cosm ###################

# Load acute and chronic tests of cosm study with Potamopyrgus
load('output/snail_2019-08-01.Rda')
pota_acute <- output_data
load('output/snail_2019-08-15.Rda')
pota_chronic <- output_data

# Summarise data for FLU
pota_acute_FLU <- summarise_data(input_data = pota_acute, chemical = 'FLU',
                                     exp_dur = 240, timebin = 10, expsr_dur = '2d')
pota_chronic_FLU <- summarise_data(input_data = pota_chronic, chemical = 'FLU', 
                                       exp_dur = 240, timebin = 10, expsr_dur = '21d')
# And make a combined heatmap of the acute and chronic tests
plot_heatmap2(input_acute = pota_acute_FLU, 
              input_chronic = pota_chronic_FLU,
              species = 'Potamopyrgus',
              chemical = 'FLU',
              exp_dur = 240,
              timebin = 10,
              test_location = 'Cosm')

# Summarise data for SMX
pota_acute_SMX <- summarise_data(input_data = pota_acute, chemical = 'SMX',
                                     exp_dur = 240, timebin = 10, expsr_dur = '2d')
pota_chronic_SMX <- summarise_data(input_data = pota_chronic, chemical = 'SMX', 
                                       exp_dur = 240, timebin = 10, expsr_dur = '21d')
# And make a combined heatmap of the acute and chronic tests
plot_heatmap2(input_acute = pota_acute_SMX, 
              input_chronic = pota_chronic_SMX,
              species = 'Potamopyrgus',
              chemical = 'SMX',
              exp_dur = 240,
              timebin = 10,
              test_location = 'Cosm')

################### Gammarus - Lab ###################

# Load acute and chronic tests of cosm study with Gammarus 
#load('output/Gammarus_2019-08-08.Rda')
load('output/Gammarus_2019-08-23.Rda')
gammarus_acute <- output_data
load('output/Gammarus_2019-10-08.Rda')
gammarus_chronic <- output_data

# Summarise data for FLU
gammarus_acute_FLU <- summarise_data(input_data = gammarus_acute, chemical = 'FLU',
                                     exp_dur = 480, timebin = 10, expsr_dur = '2d')
gammarus_chronic_FLU <- summarise_data(input_data = gammarus_chronic, chemical = 'FLU', 
                                       exp_dur = 480, timebin = 10, expsr_dur = '21d')
# And make a combined heatmap of the acute and chronic tests
plot_heatmap2(input_acute = gammarus_acute_FLU, 
              input_chronic = gammarus_chronic_FLU,
              species = 'Gammarus',
              chemical = 'FLU',
              exp_dur = 480,
              timebin = 10,
              test_location = 'Lab')
# Plot velocity
plot_velocity(input_acute = gammarus_acute_FLU,
              input_chronic = gammarus_chronic_FLU,
              species = 'Gammarus',
              chemical = 'FLU',
              exp_dur = 480,
              timebin = 10,
              test_location = 'Lab')
# Combine chronic and acute data
combined_data <- rbind(gammarus_acute, gammarus_chronic)
# Prepare data for linear mixed effect (LME) modelling
data_prepared <- prepare_lme(input_data = combined_data,
                             chemical = 'FLU',
                             exp_dur = 480,
                             timebin = 1)


data_prepared <- combined_data
# Create timebins
bins <- seq(0, 480, 10)
# Add timebins to data
combined_data$timebin <- cut(combined_data$time, bins, labels = FALSE)
# Remove NAs (time > 480)
combined_data <- combined_data[!is.na(combined_data$timebin),]
combined_data$light_on_off <- combined_data$time_bin



# Filter on light or dark period
light_periods <- unique(data$time_bin)
test <- lapply(light_periods, function(x){ filter(as_tibble(data), time_bin == x) })
test <- filter(data_prepared, light_on_off == 3)
test_model <- lme(avaspeed ~ timebin*Treatment_conc + 
      timebin*expsr_dur +
      Treatment_conc*expsr_dur+
      timebin*Treatment_conc*expsr_dur, 
    random = ~ 1+ timebin | cosm_nr/ind,
    data = test, control=ctrl)
# Fit LME model
ctrl <- lmeControl(opt='optim')
models_test <- lapply(test, function(x) lme(avaspeed ~ timebin*Treatment_conc + 
                                              timebin*expsr_dur +
                                              Treatment_conc*expsr_dur+
                                              timebin*Treatment_conc*expsr_dur, 
                                            random = ~ 1+ timebin | cosm_nr/ind,
                                            data = x, control=ctrl))
#models_test_summary <- lapply(models_test, function(x) summary(x))
models_test_anova <- lapply(models_test, function(x) anova(x))

model_1 <- lme(avaspeed ~ timebin*Treatment_conc + timebin*expsr_dur +
                 Treatment_conc*expsr_dur, 
               random = ~ 1+ timebin | cosm_nr/ind,
               data = data_prepared, control=ctrl)
# And see output
summary(model_1)
anova(model_1)
library(multcomp)
summary(glht(model_1, linfct=mcp(Treatment_conc="Tukey")))

################### Potamopyrgus - Lab ###################

# Load acute and chronic tests
load('output/snail_2019-09-06.Rda')
pota_acute_lab <- output_data
load('output/snail_2019-10-07.Rda')
pota_chronic_lab <- output_data

# Summarise data for FLU
pota_acute_lab_summarised <- summarise_data(input_data = pota_acute_lab, chemical = 'FLU',
                                     exp_dur = 240, timebin = 10, expsr_dur = '2d')
pota_chronic_lab_summarised <- summarise_data(input_data = pota_chronic_lab, chemical = 'FLU', 
                                       exp_dur = 240, timebin = 10, expsr_dur = '21d')
# And make a combined heatmap of the acute and chronic tests
plot_heatmap2(input_acute = pota_acute_lab_summarised, 
              input_chronic = pota_chronic_lab_summarised,
              species = 'Potamopyrgus',
              chemical = 'FLU',
              exp_dur = 240,
              timebin = 10,
              test_location = 'Lab')
# Plot velocity
plot_velocity(input_acute = pota_acute_lab_summarised,
              input_chronic = pota_chronic_lab_summarised,
              species = 'Potamopyrgus',
              chemical = 'FLU',
              exp_dur = 240,
              timebin = 10,
              test_location = 'Lab')
# Prepare data for linear mixed effect (LME) modelling
data_prepared <- prepare_lme(input_data = pota_chronic_lab,
                             chemical = 'FLU',
                             exp_dur = 240,
                             timebin = 10)
# Fit LME model
ctrl <- lmeControl(opt='optim')
data_grouped <- groupedData(timebin ~ Treatment_conc |cosm_nr/ind, data = data_prepared)
model_1 <- lme(mean.speed ~ timebin*Treatment_conc, 
               random = ~ 1 | cosm_nr/ind,
               data = data_prepared, control=ctrl)
# And see output
summary(model_1)
anova(model_1)
library(multcomp)
summary(glht(model_1, linfct=mcp(Treatment_conc="Tukey")))
library(emmeans)
emm = emmeans(model_1, ~ timebin*Treatment_conc)
# or for simple comparisons
pairs(emm, simple = 'each')
