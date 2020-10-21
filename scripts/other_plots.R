data_other <- data

# Lights off as 0, lights on as 1
data_other$light_on_off <- ifelse(data_other$time_bin == 1 | 
                                    data_other$time_bin == 3, 0, 1)


# Add interaction between timebin and treatment group
  data_other$combined_group <- interaction(data_other$light_on_off, 
                                           data_other$Treatment_conc)

# Calculate average speed at all time bins over all cosms/individuals
  data_other_summarised <- data_other %>% group_by(combined_group) %>%
  summarise(avaspeed = mean(aspeed),
            sdaspeed = sd(aspeed),
            light_on_off = mean(light_on_off),
            groups = mean(group),
            Treatment_conc = mean(Treatment_conc))
  
  data_other_summarised$Treatment_conc <- factor(data_other_summarised$Treatment_conc)
  # Make plot 
  p <- ggplot(data_other_summarised, aes(x=light_on_off, y=avaspeed, 
                                   group=Treatment_conc, color=Treatment_conc)) + 
    geom_line() +
    geom_point()  
    geom_errorbar(aes(ymin=avaspeed-sdaspeed, ymax=avaspeed+sdaspeed), width=.2,
                  position=position_dodge(0.05))

  p
  
  data_summarised$light_on_off <- factor(data_summarised$light_on_off)
  model_1 <- lme(mean.speed ~ timebin*Treatment_conc + timebin*light_on_off +
                   Treatment_conc*light_on_off, 
                 random = ~ 1+ timebin | cosm_nr/ind,
                 data = data_summarised, control=ctrl)
  visreg(model_1, 'timebin', by = c('light_on_off', 'Treatment_conc'), 
         overlay = TRUE, partial = TRUE)
  

  p <- ggplot(data_other_summarised, aes(x=Treatment_conc, y=avaspeed,
                              group=light_on_off, color=light_on_off)) + 
    geom_line() +
    geom_point()   
  geom_errorbar(aes(ymin=avaspeed-sdaspeed, ymax=avaspeed+sdaspeed), width=.2,
               position=position_dodge(0.05))
  
  p
  
  


