insert_BR <- function(DF, name_label, where = 12) {
  
  # DEBUG
  # DF = DF_filtered
  # name_label = "offense_type"
  # where = 12
  name_label_short = paste0(name_label, "_short")
  
  LABELS = DF %>% distinct(!!name_label:= get(name_label)) %>% pull(!!name_label)
  LABELS_spaces = stringr::str_locate_all(string = LABELS, pattern = " ")
  
  DF_LABELS = 
    1:length(LABELS_spaces) %>% 
    map_df(~
             {
               # cat(.x)
               # .x = 3
               tibble(!!name_label := LABELS[.x], WHERE = LABELS_spaces[[.x]][which.min(abs(where - LABELS_spaces[[.x]][,1]))]) %>% 
                 mutate(!!name_label_short := stringi::stri_sub_replace(get(name_label), WHERE, WHERE, value = " <br> ")) %>% 
                 select(!!name_label_short, !!name_label)
             })
  
  DF_out = DF %>% 
    left_join(DF_LABELS, by = name_label) %>% 
    mutate(!!name_label_short := 
             case_when(
               is.na(get(name_label_short)) ~ get(name_label),
               TRUE ~ get(name_label_short)
             ))
  
  return(DF_out)
  
}
