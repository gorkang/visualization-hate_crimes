plot_FBI_t1_lines <- function(DF) {
  
  # DF = DF_FBI_t1
  whitelist = DF %>% count(bias_motivation, name) %>% filter(n > 6) %>% distinct(bias_motivation)# count(n)
  
  plot_time <- function(DF) {
    DF %>% 
      filter(bias_motivation %in% whitelist$bias_motivation) %>%
      ggplot(aes(year, value, color = name, group = name)) +
      geom_point() +
      geom_line() +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 7)) +
      # scale_x_continuous(n.breaks = 14, breaks = scales::pretty_breaks()) +
      facet_wrap(~ bias_motivation, scales = "free") +
      theme_minimal() +
      theme(legend.position = "bottom") +
      labs(title = "Incidents, Offenses, Victims, and Known Offenders", 
           subtitle = "By Bias Motivation. 2006-2019") +
      ggthemes::theme_fivethirtyeight() 
      # ggthemes::scale_color_fivethirtyeight()
    
  }
  
  PLOT_general = plot_time(DF %>% filter(grepl(":", bias_motivation)))
  PLOT_specific = plot_time(DF %>% filter(!grepl(":", bias_motivation))) # scales = "free_x" & filter(!bias_motivation %in% c("Total", "Single-Bias Incidents")) %>% 
  
  ggsave("outputs/PLOTs/plot_FBI_table1_general.png", PLOT_general, dpi = 300, width = 16, height = 9)
  ggsave("outputs/PLOTs/plot_FBI_table1_specific.png", PLOT_specific, dpi = 300, width = 20, height = 12)
  
  PLOTS = list(PLOT_general = PLOT_general,
               PLOT_specific = PLOT_specific)
  
  return(PLOTS)
  
}