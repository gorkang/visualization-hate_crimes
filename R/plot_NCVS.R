plot_NCVS <- function(DF) {
  
  PLOT = DF %>% 
    # filter(sub == "total") %>% 
    ggplot(aes(year, value, color = supra, group = supra)) +
    geom_point() +
    geom_line() +
    theme_minimal() +
    scale_y_continuous(labels = scales::comma) +
    facet_wrap(~ sub, scales = "free_y")
  
  ggsave("outputs/PLOTs/plot_NCVS_hcv0519t01.png", PLOT, dpi = 300, width = 16, height = 9, bg = "white")
  
  return(PLOT)
}