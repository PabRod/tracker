### some functions needed
fac_table <- function(model_obj, digits = 2, caption = "Effect of each factor") {
  qq_anv <- anova.lme(model_obj, type = "marginal")
  
  rest_tb <-  data.frame(Factors = row.names(qq_anv)[-1],
                         Fvalue = paste0(round(qq_anv[-1, c(3)], digits), 
                                         "(", round(qq_anv[-1, c(4)], digits), ")"), 
                         stringsAsFactors = FALSE)
  
  
  rest_tb %>% knitr::kable(., digits = digits, caption = caption, align = 'c')
  #rest_tb
}

effect_plot <- function(model_obj, interest, effect = "SC - C", extra = NULL) {
  qqc_sum <- summary(model_obj)$tTable
  
  if(is.null(extra)) {
    
    test <- str_detect(row.names(qqc_sum), interest[1]) & 
      str_detect(row.names(qqc_sum), interest[2])
    
  } else {
    
    test <- (str_detect(row.names(qqc_sum), interest[1]) & 
               str_detect(row.names(qqc_sum), interest[2])) |
      row.names(qqc_sum)  == extra
    
  }
  
  needed <- qqc_sum[test, 1:2]
  
  if(class(needed) != "matrix") {
    
    pdata1 <- data.frame(Effect = effect, Value = needed[1],
                         SError = needed[2])
    
  } else {
    
    pdata1 <- data.frame(Effect = effect, Value = needed[,1],
                         SError = needed[,2])
    
  }
  pdata1 %>% ggplot(aes(x = Effect, y = Value, group = 1)) + 
    geom_errorbar(aes(ymin = Value - 1.96*SError, ymax = Value + 1.96*SError), 
                  width = 0.2, 
                  position = position_dodge(0.05), 
                  size = 1, colour = "grey40") + 
    geom_point(size = 5, fill = "yellow3", shape = 21)  + 
    theme_pubr() +
    geom_hline(yintercept = 0, linetype = "dashed", size = 2) + 
    labs(x= "Difference", y = expression(Estimate %+-% 1.96%*%SE))
}