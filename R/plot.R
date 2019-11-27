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

#' Plot heatmap
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
  data_summarised <- data %>% group_by(combined_group) %>%
    summarise(avaspeed = mean(aspeed),
              sdaspeed = sd(aspeed),
              group = mean(group),
              Treatment_conc = mean(Treatment_conc))
  
  # Convert into wide format, using 10s timebins as columns
  data_summarised <- spread(data_summarised[c(2,4,5)], group, avaspeed)
  
  # Plot heatmap
  heatmap(as.matrix(data_summarised[-1]), Colv = NA, Rowv = NA, scale = 'none',
          col = rev(heat.colors(256)))
}


#' Plot more sophisticated heatmap
#'
#' @param input_acute Dataframe of acute exposure
#' @param input_chronic Dataframe of chronic exposure
#' @param species Name of test species
#' @param chemical Name of chemical
#' @param timebin Size of the timebin in seconds
#' @param exp_dur Duration of the experiment in seconds
#'
#' @return The plot
#' @export
#'

plot_heatmap2 <- function(input_acute, input_chronic, chemical, species, exp_dur, timebin,
                          test_location){
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
  png(paste('output/Heatmap_', species, '_', chemical, '_', test_location, '.png', sep = ''),
      res = 300, height = 7, width = 10, units = 'in')
  p <- ggplot(combined_data,aes(group, Treatment_conc, fill=avaspeed))+
    geom_tile(color= "white",size=0.1) + 
    scale_fill_viridis(name="Swimming\nvelocity\n(mm/s)",option ="C")
  p <- p + facet_grid(rows = vars(expsr_dur))
  p <- p + scale_y_discrete(limits = c('200', '20', '2', '0.2', '0'))
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