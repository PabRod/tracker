# Clear environment
rm(list=ls())

# Load libraries
library(dplyr)
library(Hmisc)
library(tidyr)
library(ggplot2)
library(viridis)
library(ggExtra)

# Source all functions
sapply(list.files('R', full.names = T), source)

# Make heatmaps for Gammarus chronic
plot_heatmap(test_date = '2019-07-31', chemical = 'FLU', timebin = 10, exp_dur = 480)
plot_heatmap(test_date = '2019-07-31', chemical = 'SMX', timebin = 10, exp_dur = 480)

# Make heatmaps for Gammarus acute
plot_heatmap(test_date = '2019-08-08', chemical = 'FLU', timebin = 10, exp_dur = 480)
plot_heatmap(test_date = '2019-08-08', chemical = 'SMX', timebin = 10, exp_dur = 480)

# Plot velocity for Gammarus acute
plot_velocity(test_date = '2019-08-08', chemical = 'SMX', timebin = 10, exp_dur = 480)
plot_velocity(test_date = '2019-07-31', chemical = 'SMX', timebin = 10, exp_dur = 480)



## Make combined heatmap for Gammarus Acute and Chronic
summarise_data <- function(input_data, chemical, exp_dur, timebin, expsr_dur){
  # Only focus on one chemical
  data <- filter(input_data, Treatment_chem == chemical | Treatment_chem == 'C')
  
  # Create timebins
  bins <- seq(0, exp_dur, timebin)
  # Add timebins to data
  data$group <- cut(data$time, bins, labels = FALSE)
  # Remove NAs (time > 480)
  data <- data[!is.na(data$group),]
  # Add interaction between timebin and treatment group
  data$combined_group <- interaction(data$group, data$Treatment_conc)
  
  # Summarise by group
  data_summarised <- data %>% group_by(combined_group) %>%
    summarise(avaspeed = mean(aspeed),
              sdaspeed = sd(aspeed),
              group = mean(group),
              Treatment_conc = mean(Treatment_conc))
  data_summarised$expsr_dur <- expsr_dur
  return(data_summarised)
}

plot_heatmap2 <- function(input_acute, input_chronic, chemical, species, exp_dur, timebin){
  # Combine both dfs together
  combined_data <- rbind(input_acute, input_chronic)
  # Convert treatment conc to factor
  combined_data$Treatment_conc <- as.factor(combined_data$Treatment_conc)
  # Convert exp_dur to factor
  combined_data$expsr_dur <- factor(combined_data$expsr_dur, levels = c(input_acute$expsr_dur[1],
                                                                    input_chronic$expsr_dur[1]))
  # Multiply group with 10 to get real time in seconds
  combined_data$group <- combined_data$group*10
  
  # Create a vector that contains the color of the x-axis label
  a <- c(rep('plain', 12), rep('bold', 12), rep('plain', 12), rep('bold', 12))
  b <- c(rep('black', 12), rep('red', 12), rep('black', 12), rep('red', 12))
  
  ## Build plot
  png(paste('output/Heatmap_', species, '_', chemical, '.png', sep = ''),
      res = 300, height = 7, width = 10, units = 'in')
  p <- ggplot(combined_data,aes(group, Treatment_conc, fill=avaspeed))+
    geom_tile(color= "white",size=0.1) + 
    scale_fill_viridis(name="Swimming\nvelocity\n(mm/s)",option ="C")
  p <- p + facet_grid(rows = vars(expsr_dur))
  p <- p + scale_y_discrete(limits = c('100', '10', '1', '0.1', '0'))
  p <- p + scale_x_continuous(breaks = seq(10, exp_dur, timebin), 
                              expand = expand_scale(mult = 0, add = 0))
  p <-p + theme_minimal(base_size = 8)
  p <-p + labs(title= paste(chemical, 'exposure'), 
               x="Time, s", y= paste(chemical, "concentration"))
  p <-p + theme(legend.position = "bottom")+
    theme(plot.title=element_text(size = 12))+
    theme(axis.ticks=element_blank())+
    theme(axis.text.x=element_text(size=8, angle = 90, colour = b)) +
    theme(axis.text.y=element_text(size=10)) +
    theme(axis.title = element_text(size = 10))+
    # theme(axis.text=element_text(size=10))+
    theme(strip.background = element_rect(colour="white"),
          strip.text = element_text(size = 12, face = 'bold'))+
    theme(plot.title=element_text(hjust=0))+
    theme(legend.title=element_text(size=10))+
    theme(legend.text=element_text(size=8))+
    removeGrid()#ggExtra
  
  print(p)
  
  dev.off()
}

################### Gammarus ###################

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
              timebin = 10)

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
              timebin = 10)

################### Potamopyrgus ###################

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
              timebin = 10)

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
              timebin = 10)
