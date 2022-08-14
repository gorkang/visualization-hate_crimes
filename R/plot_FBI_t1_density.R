plot_FBI_t1_density <- function(DF) {
  
  # color_points = heat.colors(14)
  unique_years = length(unique(DF$year))
  color_points = colorspace::sequential_hcl(unique_years)
  
  
  XX = DF %>% 
    filter(!grepl(":", bias_motivation)) %>% 
    filter(!bias_motivation %in% c("Total", "Single-Bias Incidents")) %>% 
    filter(name == "victims") %>% 
    mutate(year = as.character(year))
  # filter(!bias_motivation %in% c("Disability:"))
  
  # https://www.youtube.com/watch?v=war1H2xxazQ
  PLOT_ridges = ggplot(XX, aes(value, bias_motivation, fill = bias_motivation, point_color = year, group = bias_motivation)) + 
    # ggridges::geom_density_ridges(stat = "binline", bins = 20, scale = 0.95, draw_baseline = FALSE) +
    ggridges::geom_density_ridges(jittered_points = TRUE, position = "raincloud", alpha = 0.7, scale = 2) +
    theme_minimal() +
    # theme(legend.position = "bottom") +
    scale_discrete_manual("point_color", values = color_points) +
    guides(fill = "none", color = "none", group = "none")
  # guide = "bottom"
  
  ggsave("outputs/PLOTs/plot_FBI_table1_ridges.png", PLOT_ridges, dpi = 300, width = 16, height = 9, bg = "white")

  return(PLOT_ridges)
}