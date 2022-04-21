prepare_data_FBI_t1_surface <- function(DF, supra_sub = "sub", filter_bias_supra, filter_bias_motivation, absolute_relative = "absolute", arrange_by = "mean_value") {
  
  # DEBUG
    # DF = DF_FBI_t1
    # supra_sub = "sub"
    # filter_bias_supra = "*"
    # filter_bias_motivation = NULL
    # absolute_relative = "absolute"
    # arrange_by = "bias_motivation"
  
  # CHECK
  arrange_by_allowed = c("bias_motivation", "bias_supra", "mean_value")
  if (!arrange_by %in% arrange_by_allowed) cli::cli_abort("arrange_by needs to be one of: {arrange_by_allowed}")
  if (arrange_by == "mean_value") arrange_by = "desc(mean_value)"
  
  # filter_bias_supra = "Race/Ethnicity/Ancestry:"
  # absolute_relative = "relative"
  
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
      
      filter(!bias_motivation %in% c("Total", "Single-Bias")) %>% # Avoid total categories (Multiple-bias does not contain anything)
      filter(!bias_motivation %in% c("Asian/Pacific Islander", "Asian", "Native Hawaiian/Other Pacific Islander", "Arab")) %>% 
      filter(bias_motivation %in% num_years_filter$bias_motivation) %>%
      filter(name == "incidents")
    
  } else if (!is.null(filter_bias_motivation)) {
    DF_filtered = 
      DF %>% 
      # SUPRA FILTER
      # filter(bias_supra %in% c(filter_bias_supra)) %>%
      
      # Use supra or sub categories
      {if (supra_sub == "sub") filter(., !grepl(":", bias_motivation)) else filter(., grepl(":", bias_motivation))} %>% 
      
      filter(!bias_motivation %in% c("Total", "Single-Bias")) %>% # Avoid total categories (Multiple-bias does not contain anything)
      filter(!bias_motivation %in% c("Asian/Pacific Islander", "Asian", "Native Hawaiian/Other Pacific Islander", "Arab")) %>% 
      filter(bias_motivation %in% num_years_filter$bias_motivation) %>%
      filter(name == "incidents")
    
  }
  
  
  if (absolute_relative == "relative") {
    # DF_filtered %>% distinct(bias_motivation)
    
    # DATA APPROXIMATION FROM 2020
    total_population = 330000000
    DF_pct_population = 
      tibble(bias_motivation = c("White", "African American", "American Indian/Alaska Native", "Multiple Races, Group", "Hispanic/Latino"),
           `2006` = c(.76, .13, .013, .025, .18),
           `2007` = c(.76, .13, .013, .025, .18),
           `2008` = c(.76, .13, .013, .025, .18),
           `2009` = c(.76, .13, .013, .025, .18),
           `2010` = c(.76, .13, .013, .025, .18),
           `2011` = c(.76, .13, .013, .025, .18),
           `2012` = c(.76, .13, .013, .025, .18),
           `2013` = c(.76, .13, .013, .025, .18),
           `2014` = c(.76, .13, .013, .025, .18),
           `2015` = c(.76, .13, .013, .025, .18),
           `2016` = c(.76, .13, .013, .025, .18),
           `2017` = c(.76, .13, .013, .025, .18),
           `2018` = c(.76, .13, .013, .025, .18),
           `2019` = c(.76, .13, .013, .025, .18)) %>% 
      pivot_longer(cols = `2006`:`2019`, names_to = "year", values_to = "pct_population") %>% 
      mutate(year = as.integer(year))

    DF_filtered =
      DF_filtered %>% 
      left_join(DF_pct_population, by = c("bias_motivation", "year")) %>% 
      mutate(value = 
               case_when(
                 bias_motivation == "White" ~ ((value / (total_population * pct_population)) * 100000),
                 bias_motivation == "African American" ~ ((value / (total_population * pct_population)) * 100000),
                 bias_motivation == "American Indian/Alaska Native" ~ ((value / (total_population * pct_population)) * 100000),
                 bias_motivation == "Multiple Races, Group" ~ ((value / (total_population * pct_population)) * 100000),
                 bias_motivation == "Hispanic/Latino" ~ ((value / (total_population * pct_population)) * 100000),
                 TRUE ~ NA_real_
               )) %>% 
      select(-pct_population)
    # DF_filtered %>% distinct(year, bias_motivation, order) %>% pivot_wider(names_from = year, values_from = order)
  }
  
  # WIDE --------------------------------------------------------------------
  
  # Mean per group/year to order the plot
  DF_mean = DF_filtered %>% group_by(bias_motivation) %>% summarise(mean_value = mean(value))# %>% arrange(desc(mean_value))
  # arrange_by = "mean_value"
  # arrange_by = "desc(mean_value)"
  # arrange_by = "bias_motivation"
  # arrange_by = "bias_supra"
  
  
  DF_wide =
    DF_filtered %>%
    select(bias_motivation, year, value, bias_supra) %>% 
    left_join(DF_mean, by = "bias_motivation") %>%
    
    # Arrange by value. IMPORTANT: arrange(year) must be last arrange
    
    arrange(!!!rlang::parse_exprs(arrange_by)) %>% select(-mean_value, -bias_supra) %>% 
    arrange(year) %>%
    
    pivot_wider(names_from = year, values_from = "value") 
    
    # Truncate long names. TODO: better to manually set names?
    # mutate(bias_motivation_short = 
    #          case_when(
    #            str_length(bias_motivation) > 16 ~  paste0(stringr::str_sub(bias_motivation, end = 16), "..."),
    #            TRUE ~ bias_motivation
    #          ))
  
  
  # REVIEW: Insert BR to add line jump to long labels
  DF_wide = insert_BR(DF_wide, name_label = "bias_motivation", where = 16)

  # Matrix ------------------------------------------------------------------
  
  DF_matrix = 
    DF_wide %>% 
    select(-bias_motivation, -bias_motivation_short) %>% 
    as.matrix() %>% 
    magrittr::set_rownames(DF_wide$bias_motivation_short) 
  
  
  return(DF_matrix)
  
}