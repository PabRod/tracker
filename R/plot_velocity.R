#' Plot positions
#'
#' @param test_date Date of the experiment
#' @param chemical Name of chemical
#' @param timebin Size of the timebin
#' @param exp_dur Duration of the experiment in seconds
#'
#' @return The plot
#' @export
#'

plot_velocity <- function(test_date, chemical, timebin, exp_dur){
  # Load input data
  load(paste('output/', test_date, '.Rda', sep = ''))
  
  # Only focus on one chemical
  data <- filter(output_data, Treatment_chem == chemical | Treatment_chem == 'C')
  
  # Create timebins
  bins <- seq(0, exp_dur, timebin)
  # Add timebins to data
  data$group <- cut(data$time, bins, labels = FALSE)
  # Remove NAs (time > 480)
  data <- data[!is.na(data$group),]
  # Add interaction between timebin and treatment group
  data$combined_group <- interaction(data$group, data$Treatment_conc)
  
  # Calculate average speed at all time bins over all cosms/individuals
  data_summarised <- data %>% group_by(combined_group) %>%
    summarise(avaspeed = mean(aspeed),
              sdaspeed = sd(aspeed),
              time_bins = mean(group),
              Treatment_conc = mean(Treatment_conc))
  
  # Calculate 95% Confidence Interval
  data_summarised$error <- qnorm(0.975)*data_summarised$sdaspeed/sqrt(30)
  # Convert concentration into factor
  data_summarised$Treatment_conc <- as.factor(data_summarised$Treatment_conc)
  # Make plot (Super large variation, so turn errorbars on or off)
  p <- ggplot(data_summarised, aes(x=time_bins, y=avaspeed, 
                                   group=Treatment_conc, color=Treatment_conc)) + 
    geom_line() +
    geom_point() +
  geom_errorbar(aes(ymin=avaspeed-error, ymax=avaspeed+error), width=.2,
                position=position_dodge(0.05))
  p
}
