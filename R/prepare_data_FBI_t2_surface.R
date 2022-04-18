prepare_data_FBI_t2_surface <- function(DF, supra_sub = "sub", filter_offense_supra, filter_offense_type) {
  
  # DEBUG
  # DF = DF_FBI_t2
  # supra_sub = "sub"
  # filter_offense_supra = "*"
  # filter_offense_type = NULL
  
  # Key to include all
  if (filter_offense_supra == "*") {
    filter_offense_supra = 
      DF %>% 
      distinct(offense_supra) %>% 
      drop_na() %>% 
      filter(!offense_supra %in% c("Total", "Single-offense:")) %>% 
      pull(offense_supra)
  }
  
  # offense_type arranged by most incidents
  # DF %>%
  # filter(!grepl(":", offense_type)) %>%
  # filter(!offense_type %in% c("Total", "Single-offense Incidents")) %>%
  # group_by(offense_type) %>%
  # summarize(MEAN = mean(value)) %>% arrange(desc(MEAN))
  
  
  # FILTERS -----------------------------------------------------------------
  
  # Use only those with n years in the series (14 == no missing data)
  num_years_filter = 
    DF %>% 
    count(offense_type, year) %>% 
    count(offense_type) %>% 
    filter(n %in% c(5, 7, 14))
  
  if (!is.null(filter_offense_supra)) {
    DF_filtered = 
      DF %>% 
      # SUPRA FILTER
      filter(offense_supra %in% c(filter_offense_supra)) %>%
      
      # Use supra or sub categories
      {if (supra_sub == "sub") filter(., !grepl(":", offense_type)) else filter(., grepl(":", offense_type))} %>% 
      
      # filter(grepl(":", offense_type)) %>% # Keep only supra
      filter(!offense_type %in% c("Total", "Single-offense")) %>% # Avoid total categories (Multiple-offense does not contain anything)
      filter(!offense_type %in% c("Asian/Pacific Islander", "Asian", "Native Hawaiian/Other Pacific Islander", "Arab")) %>% 
      filter(offense_type %in% num_years_filter$offense_type) %>%
      filter(name == "incidents")
    
  } else if (!is.null(filter_offense_type)) {
    DF_filtered = 
      DF %>% 
      # SUPRA FILTER
      # filter(offense_supra %in% c("Gender:")) %>%
      # filter(!grepl(":", offense_type)) %>% # Avoid supra
      {if (supra_sub == "sub") filter(., !grepl(":", offense_type)) else filter(., grepl(":", offense_type))} %>% 
      # filter(grepl(":", offense_type)) %>% # Keep only supra
      filter(!offense_type %in% c("Total", "Single-offense")) %>% # Avoid total categories (Multiple-offense does not contain anything)
      filter(!offense_type %in% c("Asian/Pacific Islander", "Asian", "Native Hawaiian/Other Pacific Islander", "Arab")) %>% 
      filter(offense_type %in% num_years_filter$offense_type) %>%
      filter(name == "incidents")
    
  }
  
  DF_filtered %>% distinct(year, offense_type, order) %>% pivot_wider(names_from = year, values_from = order)
  
  
  # WIDE --------------------------------------------------------------------
  
  DF_wide =
    DF_filtered %>%
    select(offense_type, year, value) %>% 
    
    # Arrange by value. IMPORTANT: arrange(year) must be last arrange
    arrange(desc(value)) %>%
    arrange(year) %>%
    pivot_wider(names_from = year, values_from = "value")
    
    # Truncate long names. TODO: better to manually set names?
    # mutate(offense_type_short = 
    #          case_when(
    #            str_length(offense_type) > 16 ~  paste0(str_sub(offense_type, end = 16), "..."),
    #            TRUE ~ offense_type
    #          ))
  
  DF_wide = insert_BR(DF_wide, name_label = "offense_type", where = 16)
  
  
  
  # Matrix ------------------------------------------------------------------
  
  DF_matrix = 
    DF_wide %>% 
    select(-offense_type, -offense_type_short) %>% 
    as.matrix() %>% 
    magrittr::set_rownames(DF_wide$offense_type_short) 
  
  
  return(DF_matrix)
  
}