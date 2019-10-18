# Plotting functions


#' Plot positions
#'
#' @param data_loc Clean dataframe corresponding to a location
#' @param type Type of plot (available types: 'scatter', 'density')
#'
#' @return The plot
#' @export
#'
plot_positions <- function(data_loc, type='scatter') {
  
  if (type=='scatter') {
    p <- plot(data_loc$x, data_loc$y, pch=19, col=rgb(0, 0, 0, 0.15))
    return(p)
  } else if (type == 'density') {
    if(nrow(data_loc)>5){
      if(sd(data_loc$x)>1){
        k <- kde2d(data_loc$x, data_loc$y, n=100)
        p <- image(k, col = topo.colors(100))
        return(p)
      }
    }
  }
}

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

plot_heatmap <- function(test_date, chemical, timebin, exp_dur){
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
  
  # Summarise by group
  summarised_data <- data %>% group_by(combined_group) %>%
    summarise(mean.speed = mean(aspeed),
              group = mean(group),
              Treatment_conc = mean(Treatment_conc))
  
  # Convert into wide format, using 10s timebins as columns
  summarised_data <- spread(summarised_data[2:4], group, mean.speed)
  
  # Plot heatmap
  heatmap(as.matrix(summarised_data[-1]), Colv = NA, Rowv = NA, scale = 'none',
          col = rev(heat.colors(256)))
}

