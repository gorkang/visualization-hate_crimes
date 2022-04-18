prepare_data_FBI_t1_surface <- function(DF, supra_sub = "sub", filter_bias_supra, filter_bias_motivation) {
  
  # DEBUG
    # DF = DF_FBI_t1
    # supra_sub = "sub"
    # filter_bias_supra = "*"
    # filter_bias_motivation = NULL
  
  # Key to include all
  if (filter_bias_supra == "*") {
    filter_bias_supra = 
      DF %>% 
      distinct(bias_supra) %>% 
      drop_na() %>% 
      filter(!bias_supra %in% c("Total", "Single-Bias:")) %>% 
      pull(bias_supra)
  }
  
  # bias_motivation arranged by most incidents
  # DF %>%
  # filter(!grepl(":", bias_motivation)) %>%
  # filter(!bias_motivation %in% c("Total", "Single-Bias Incidents")) %>%
  # group_by(bias_motivation) %>%
  # summarize(MEAN = mean(value)) %>% arrange(desc(MEAN))
  
  
  # FILTERS -----------------------------------------------------------------
  
  # Use only those with n years in the series (14 == no missing data)
  num_years_filter = 
    DF %>% 
    count(bias_motivation, year) %>% 
    count(bias_motivation) %>% 
    filter(n %in% c(5, 7, 14))
  
  if (!is.null(filter_bias_supra)) {
    DF_filtered = 
      DF %>% 
      # SUPRA FILTER
      filter(bias_supra %in% c(filter_bias_supra)) %>%
      
      # Use supra or sub categories
      {if (supra_sub == "sub") filter(., !grepl(":", bias_motivation)) else filter(., grepl(":", bias_motivation))} %>% 
      
      # filter(grepl(":", bias_motivation)) %>% # Keep only supra
      filter(!bias_motivation %in% c("Total", "Single-Bias")) %>% # Avoid total categories (Multiple-bias does not contain anything)
      filter(!bias_motivation %in% c("Asian/Pacific Islander", "Asian", "Native Hawaiian/Other Pacific Islander", "Arab")) %>% 
      filter(bias_motivation %in% num_years_filter$bias_motivation) %>%
      filter(name == "incidents")
    
  } else if (!is.null(filter_bias_motivation)) {
    DF_filtered = 
      DF %>% 
      # SUPRA FILTER
      # filter(bias_supra %in% c("Gender:")) %>%
      # filter(!grepl(":", bias_motivation)) %>% # Avoid supra
      {if (supra_sub == "sub") filter(., !grepl(":", bias_motivation)) else filter(., grepl(":", bias_motivation))} %>% 
      # filter(grepl(":", bias_motivation)) %>% # Keep only supra
      filter(!bias_motivation %in% c("Total", "Single-Bias")) %>% # Avoid total categories (Multiple-bias does not contain anything)
      filter(!bias_motivation %in% c("Asian/Pacific Islander", "Asian", "Native Hawaiian/Other Pacific Islander", "Arab")) %>% 
      filter(bias_motivation %in% num_years_filter$bias_motivation) %>%
      filter(name == "incidents")
    
  }
  
  # DF_filtered %>% distinct(year, bias_motivation, order) %>% pivot_wider(names_from = year, values_from = order)
  
  
  # WIDE --------------------------------------------------------------------
  
  DF_wide =
    DF_filtered %>%
    select(bias_motivation, year, value) %>% 
    
    # Arrange by value. IMPORTANT: arrange(year) must be last arrange
    arrange(desc(value)) %>%
    arrange(year) %>%
    pivot_wider(names_from = year, values_from = "value") 
    
    # Truncate long names. TODO: better to manually set names?
    # mutate(bias_motivation_short = 
    #          case_when(
    #            str_length(bias_motivation) > 16 ~  paste0(stringr::str_sub(bias_motivation, end = 16), "..."),
    #            TRUE ~ bias_motivation
    #          ))
  
  
  
  DF_wide = insert_BR(DF_wide, name_label = "bias_motivation", where = 16)

  # Matrix ------------------------------------------------------------------
  
  DF_matrix = 
    DF_wide %>% 
    select(-bias_motivation, -bias_motivation_short) %>% 
    as.matrix() %>% 
    magrittr::set_rownames(DF_wide$bias_motivation_short) 
  
  
  return(DF_matrix)
  
}