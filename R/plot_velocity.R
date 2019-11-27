#' Plot positions
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

plot_velocity <- function(input_acute, input_chronic, chemical, species, exp_dur, timebin,
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
  
  # Convert concentration into factor
  combined_data$Treatment_conc <- as.factor(combined_data$Treatment_conc)
  
  ## Build plot
  png(paste('output/Velocity_', species, '_', chemical, '_', test_location, '.png', sep = ''),
      res = 300, height = 7, width = 10, units = 'in')
  p <- ggplot(combined_data,aes(x = group, y = avaspeed, group=Treatment_conc,
                                color = Treatment_conc))
  p <- p+ geom_rect(fill = 'lightgrey', xmin = -Inf, xmax = 120, ymin = -Inf, ymax = Inf, 
                    alpha = 0.05, linetype = 'blank')
  p <- p+ geom_rect(fill = 'lightgrey', xmin = 240, xmax = 360, ymin = -Inf, ymax = Inf, 
                    alpha = 0.05, linetype = 'blank')
  p <- p+ geom_line()+geom_point()
  #geom_tile(color= "white",size=0.1) +
  #scale_fill_viridis(name="Swimming\nvelocity\n(mm/s)",option ="C")
  p <- p + facet_grid(rows = vars(expsr_dur))
  #p <- p + scale_y_discrete(limits = c('200', '20', '2', '0.2', '0'))
  p <- p + scale_x_continuous(breaks = seq(10, exp_dur, timebin),
                              expand = expand_scale(mult = 0, add = 0))
  p <- p + theme_bw()
  #p <-p + theme_minimal(base_size = 8)
  p <-p + labs(title= paste(chemical, 'exposure'),
               x="Time, s", y= 'Velocity, mm/s')
  p <-p + theme(legend.position = "bottom")+
    theme(plot.title=element_text(size = 12))+
    theme(axis.ticks.x=element_blank())+
    theme(axis.text.x=element_text(size=8, angle = 90, colour = b, vjust = 0.2)) +
    theme(axis.text.y=element_text(size=10)) +
    theme(axis.title = element_text(size = 10))+
    # theme(axis.text=element_text(size=10))+
    theme(#strip.background = element_rect(colour="white"),
      strip.text = element_text(size = 12, face = 'bold'))+
    theme(plot.title=element_text(hjust=0))+
    theme(legend.title=element_text(size=10))+
    theme(legend.text=element_text(size=8))+
    removeGrid()#ggExtra
  
  print(p)
  
  dev.off()
}
