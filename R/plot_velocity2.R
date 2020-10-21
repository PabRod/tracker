#' Plot positions
#'
#' @param input1 First dataframe with behavioural data
#' @param input2 Second dataframe with behavioural data
#' @param figure_name Name of the output figure
#' @param group_id Columnname used for grouping
#' @param compare_id Columnname used for comparing

#'
#' @return The plot
#' @export
#'

plot_velocity2 <- function(input_data, #figure_name,
                           group_id, compare_id){
                          #species, exp_dur, timebin,
                          #test_location){
  # Combine both dfs together
  #combined_data <- rbind(input1, input2)
  
  combined_data <- input_data %>% 
    group_by_at(c(group_id, 'bins', compare_id)) %>%
    summarise(avlaspeed = mean(avlaspeed), 
              vavlaspeed = var(vavlaspeed),
              number = n()) %>% 
    mutate(selaspeed = vavlaspeed/number)
  
  ## Extra figure, figure 1 - fluoxetine concentrations --> standardized data
  # combined_data_control <- combined_data %>%
  #   dplyr::filter(Treatment_conc == 0)
  # combined_data_others <- combined_data %>%
  #   dplyr::filter(Treatment_conc != 0)
  # splitted_data <- split(combined_data_others, combined_data_others$Treatment_conc)
  # splitted_data_standardized <- lapply(splitted_data[2:5], function(x){
  #   x$standardized <- x$avlaspeed - combined_data_control$avlaspeed; x
  # })
  # combined_data <- do.call(rbind, splitted_data_standardized)
  
  # Convert treatment conc to factor
  #combined_data$Treatment_conc <- as.factor(combined_data$Treatment_conc)
  # Convert exp_dur to factor
  #combined_data$expsr_dur <- factor(combined_data$expsr_dur, levels = c(input1$expsr_dur[1],
  #                                                                      input2c$expsr_dur[1]))
  # Multiply group with 10 to get real time in seconds
  combined_data$time <- combined_data$bins*10
  # Add experiment identifier
  #combined_data$experiment <- paste(combined_data$test_location,
  #                                  combined_data$test_duration)
  
  # Create a vector that contains the color of the x-axis label
  a <- c(rep('plain', 12), rep('bold', 12), rep('plain', 12), rep('bold', 12))
  b <- c(rep('black', 12), rep('red', 12), rep('black', 12), rep('red', 12))
  
  # Convert concentration into factor
  #combined_data$Treatment_conc <- as.factor(combined_data$Treatment_conc)
  
  ## Build plot
  #png(paste('./../output/',figure_name,'.png', sep = ''),
  #    res = 300, height = 7, width = 10, units = 'in')
  p <- ggplot(combined_data, aes_string(x = 'time', 
                                        y = 'avlaspeed', #standardized
                                 group = compare_id,
                                 color = compare_id))
  p <- p+ geom_rect(fill = 'lightgrey', xmin = -Inf, xmax = 120, ymin = -Inf, ymax = Inf, 
                    alpha = 0.05, linetype = 'blank')
  p <- p+ geom_rect(fill = 'lightgrey', xmin = 240, xmax = 360, ymin = -Inf, ymax = Inf, 
                    alpha = 0.05, linetype = 'blank')
  p <- p+ geom_line()#+geom_point()
  p <- p+ geom_errorbar(aes(ymin=avlaspeed-vavlaspeed, ## don't use with standardized figure
                            ymax=avlaspeed+vavlaspeed))
  #geom_tile(color= "white",size=0.1) +
  #scale_fill_viridis(name="Swimming\nvelocity\n(mm/s)",option ="C")
  grid_function <- as.formula(
    ifelse(length(group_id) == 2,
           paste(group_id[[1]], '~', group_id[[2]], sep = ''),
           paste(group_id, '~ .', sep = '')))
  p <- p + facet_grid(grid_function)#rows = vars(experiment)
  #p <- p + scale_y_discrete(limits = c('200', '20', '2', '0.2', '0'))
  p <- p + scale_x_continuous(breaks = seq(10, 480, 20),
                              expand = expansion(mult = 0, add = 0))
  p <- p + theme_bw()
  #p <-p + theme_minimal(base_size = 8)
  p <-p + labs(#title= paste(chemical, 'exposure'),
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
  
  #dev.off()
}
