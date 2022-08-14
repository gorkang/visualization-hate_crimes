prepare_data_FBI_t1_surface <- function(DF, supra_sub = "sub", filter_bias_supra, filter_bias_motivation, absolute_relative = "absolute", arrange_by = "mean_value", variable = "victims") {
  
  DF %>% count(name)
  # DEBUG
    # DF = DF_FBI_t1
    # supra_sub = "sub"
    # filter_bias_supra = "*"
    # variable = "victims"
    # filter_bias_motivation = NULL
    # absolute_relative = "relative"
    # arrange_by = "bias_motivation"
  
  # filter_bias_supra = "Sexual Orientation:"
  # filter_bias_supra = "Gender Identity:"
  # CHECK
  arrange_by_allowed = c("bias_motivation", "bias_supra", "mean_value")
  if (!arrange_by %in% arrange_by_allowed) cli::cli_abort("arrange_by needs to be one of: {arrange_by_allowed}")
  if (arrange_by == "mean_value") arrange_by = "desc(mean_value)"
  
  
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
  # num_years_filter = 
  #   DF %>% 
  #   count(bias_motivation, year) %>% 
  #   count(bias_motivation) %>% 
  #   filter(n %in% c(5, 7, 14))
  
  if (!is.null(filter_bias_supra)) {
    DF_filtered = 
      DF %>% 
      # SUPRA FILTER
      filter(bias_supra %in% c(filter_bias_supra)) %>%
      
      # Use supra or sub categories
      {if (supra_sub == "sub") filter(., !grepl(":", bias_motivation)) else filter(., grepl(":", bias_motivation))} %>% 
      
      filter(!bias_motivation %in% c("Total", "Single-Bias")) %>% # Avoid total categories (Multiple-bias does not contain anything)
      # filter(!bias_motivation %in% c("Asian/Pacific Islander", "Asian", "Native Hawaiian/Other Pacific Islander", "Arab")) %>% 
      # filter(bias_motivation %in% num_years_filter$bias_motivation) %>%
      filter(name == variable) # incidents, victims...
    
  } else if (!is.null(filter_bias_motivation)) {
    DF_filtered = 
      DF %>% 
      # SUPRA FILTER
      # filter(bias_supra %in% c(filter_bias_supra)) %>%
      
      # Use supra or sub categories
      {if (supra_sub == "sub") filter(., !grepl(":", bias_motivation)) else filter(., grepl(":", bias_motivation))} %>% 
      
      filter(!bias_motivation %in% c("Total", "Single-Bias")) %>% # Avoid total categories (Multiple-bias does not contain anything)
      filter(!bias_motivation %in% c("Asian/Pacific Islander", "Asian", "Native Hawaiian/Other Pacific Islander")) %>% #, "Arab"
      filter(bias_motivation %in% num_years_filter$bias_motivation) %>%
      filter(name == variable) # incidents, victims...
    
  }
  
  
  if (absolute_relative == "relative") {
    # DF %>% filter(bias_supra == "Religion:") %>% 
    # distinct(bias_motivation, .keep_all = T) %>% arrange(bias_supra, bias_motivation) %>% pull(bias_motivation) # %>% cat()
    
    # REVIEW: DATA IS APPROXIMATION FROM 2020 CENSUS, and OTHER SOURCES!!! 
    
      # *** Need to get data for each year! ***
      
      # These seem to be combined in the latter years
        # "Asian",
        # "Native Hawaiian/Other Pacific Islander",
        # "Asian/Pacific Islander",
      
      # No good sources
      # "Other Race/Ethnicity/Ancestry",
      # "Physical",
      # "Mental",
      
    cli::cli_alert_danger("Census data is a single snapshot of 2020, including other sources. Relative info is AN APROXIMATION!")
      
      total_population = 330000000
      
      years_DF = unique(DF_filtered$year)
      
      # Create a demographic DF.
      DF_inicial = tibble(bias_motivation = 
                            # Gender
                            c("Female", "Male",
                              # Disability: (???) https://data.census.gov/cedsci/table?q=Disability&d=ACS%205-Year%20Estimates%20Subject%20Tables&tid=ACSST5Y2020.S1810&moe=false
                              "Physical", "Mental", 
                              # Race:
                              "Arab", "White", "African American", "American Indian/Alaska Native", "Multiple Races, Group", 
                              # Ethnicity:
                              "Hispanic/Latino", "Not Hispanic/Latino",
                              # Gender Identity:
                              "Transgender", "Gender Non-Conforming", 
                              # Sexual orientation:
                              "Heterosexual", "Bisexual", "Lesbian, Gay, Bisexual,/Transgender (Mixed Group)", "I don't know",  "Gay (Male)", "Lesbian",
                              # Religion: # https://www.pewresearch.org/religion/religious-landscape-study/
                              "Atheism/Agnosticism/etc.", "Buddhist", "Catholic",  "Eastern Orthodox (Russian, Greek, Other)", "Hindu", "Islamic (Muslim)", "Jehovah's Witness", "Jewish", "Mormon", "Multiple Religions, Group", "Other Christian", "Other Religion", "Protestant", "Sikh"))
      
      # WE USE DATA FROM CENSUS 2020 and other sources as a proof of concept
      DF_pct_population = DF_inicial |> 
        add_column(
          1:length(years_DF) |> 
            map_dfc(~ {
              tibble("{years_DF[.x]}" := 
                       c(.508, .492, 0.06, 0.051, .0064, .76, .13, .013, .025, .18, .6, 0.006, 0.017, 0.878, 0.059, 0.022, 0.019, 0.045, 0.022, .228, 0.007, .208, 0.005, 0.007, 0.009, 0.008, 0.019, 0.016, NA, 0.004, 0.015, 0.254+0.147+0.065, 0.001515152))
              
            })
        ) %>% 
        pivot_longer(cols = matches("[0-9]{4}"), names_to = "year", values_to = "pct_population") %>% 
        mutate(year = as.integer(year))
      
      # DF_pct_population = 
      # tibble(bias_motivation = 
      #           # Gender
      #          c("Female", "Male",
      #           # Disability: (???) https://data.census.gov/cedsci/table?q=Disability&d=ACS%205-Year%20Estimates%20Subject%20Tables&tid=ACSST5Y2020.S1810&moe=false
      #           "Physical", "Mental", 
      #           # Race:
      #           "Arab", "White", "African American", "American Indian/Alaska Native", "Multiple Races, Group", 
      #           # Ethnicity:
      #           "Hispanic/Latino", "Not Hispanic/Latino",
      #           # Gender Identity:
      #           "Transgender", "Gender Non-Conforming", 
      #           # Sexual orientation:
      #           "Heterosexual", "Bisexual", "Lesbian, Gay, Bisexual,/Transgender (Mixed Group)", "I don't know",  "Gay (Male)", "Lesbian",
      #           # Religion: # https://www.pewresearch.org/religion/religious-landscape-study/
      #           "Atheism/Agnosticism/etc.", "Buddhist", "Catholic",  "Eastern Orthodox (Russian, Greek, Other)", "Hindu", "Islamic (Muslim)", "Jehovah's Witness", "Jewish", "Mormon", "Multiple Religions, Group", "Other Christian", "Other Religion", "Protestant", "Sikh"),
      #      `2006` = c(.508, .492, 0.06, 0.051, .0064, .76, .13, .013, .025, .18, .6, 0.006, 0.017, 0.878, 0.059, 0.022, 0.019, 0.045, 0.022, .228, 0.007, .208, 0.005, 0.007, 0.009, 0.008, 0.019, 0.016, NA, 0.004, 0.015, 0.254+0.147+0.065, 0.001515152),
      #      `2007` = c(.508, .492, 0.06, 0.051, .0064, .76, .13, .013, .025, .18, .6, 0.006, 0.017, 0.878, 0.059, 0.022, 0.019, 0.045, 0.022, .228, 0.007, .208, 0.005, 0.007, 0.009, 0.008, 0.019, 0.016, NA, 0.004, 0.015, 0.254+0.147+0.065, 0.001515152),
      #      `2008` = c(.508, .492, 0.06, 0.051, .0064, .76, .13, .013, .025, .18, .6, 0.006, 0.017, 0.878, 0.059, 0.022, 0.019, 0.045, 0.022, .228, 0.007, .208, 0.005, 0.007, 0.009, 0.008, 0.019, 0.016, NA, 0.004, 0.015, 0.254+0.147+0.065, 0.001515152),
      #      `2009` = c(.508, .492, 0.06, 0.051, .0064, .76, .13, .013, .025, .18, .6, 0.006, 0.017, 0.878, 0.059, 0.022, 0.019, 0.045, 0.022, .228, 0.007, .208, 0.005, 0.007, 0.009, 0.008, 0.019, 0.016, NA, 0.004, 0.015, 0.254+0.147+0.065, 0.001515152),
      #      `2010` = c(.508, .492, 0.06, 0.051, .0064, .76, .13, .013, .025, .18, .6, 0.006, 0.017, 0.878, 0.059, 0.022, 0.019, 0.045, 0.022, .228, 0.007, .208, 0.005, 0.007, 0.009, 0.008, 0.019, 0.016, NA, 0.004, 0.015, 0.254+0.147+0.065, 0.001515152),
      #      `2011` = c(.508, .492, 0.06, 0.051, .0064, .76, .13, .013, .025, .18, .6, 0.006, 0.017, 0.878, 0.059, 0.022, 0.019, 0.045, 0.022, .228, 0.007, .208, 0.005, 0.007, 0.009, 0.008, 0.019, 0.016, NA, 0.004, 0.015, 0.254+0.147+0.065, 0.001515152),
      #      `2012` = c(.508, .492, 0.06, 0.051, .0064, .76, .13, .013, .025, .18, .6, 0.006, 0.017, 0.878, 0.059, 0.022, 0.019, 0.045, 0.022, .228, 0.007, .208, 0.005, 0.007, 0.009, 0.008, 0.019, 0.016, NA, 0.004, 0.015, 0.254+0.147+0.065, 0.001515152),
      #      `2013` = c(.508, .492, 0.06, 0.051, .0064, .76, .13, .013, .025, .18, .6, 0.006, 0.017, 0.878, 0.059, 0.022, 0.019, 0.045, 0.022, .228, 0.007, .208, 0.005, 0.007, 0.009, 0.008, 0.019, 0.016, NA, 0.004, 0.015, 0.254+0.147+0.065, 0.001515152),
      #      `2014` = c(.508, .492, 0.06, 0.051, .0064, .76, .13, .013, .025, .18, .6, 0.006, 0.017, 0.878, 0.059, 0.022, 0.019, 0.045, 0.022, .228, 0.007, .208, 0.005, 0.007, 0.009, 0.008, 0.019, 0.016, NA, 0.004, 0.015, 0.254+0.147+0.065, 0.001515152),
      #      `2015` = c(.508, .492, 0.06, 0.051, .0064, .76, .13, .013, .025, .18, .6, 0.006, 0.017, 0.878, 0.059, 0.022, 0.019, 0.045, 0.022, .228, 0.007, .208, 0.005, 0.007, 0.009, 0.008, 0.019, 0.016, NA, 0.004, 0.015, 0.254+0.147+0.065, 0.001515152),
      #      `2016` = c(.508, .492, 0.06, 0.051, .0064, .76, .13, .013, .025, .18, .6, 0.006, 0.017, 0.878, 0.059, 0.022, 0.019, 0.045, 0.022, .228, 0.007, .208, 0.005, 0.007, 0.009, 0.008, 0.019, 0.016, NA, 0.004, 0.015, 0.254+0.147+0.065, 0.001515152),
      #      `2017` = c(.508, .492, 0.06, 0.051, .0064, .76, .13, .013, .025, .18, .6, 0.006, 0.017, 0.878, 0.059, 0.022, 0.019, 0.045, 0.022, .228, 0.007, .208, 0.005, 0.007, 0.009, 0.008, 0.019, 0.016, NA, 0.004, 0.015, 0.254+0.147+0.065, 0.001515152),
      #      `2018` = c(.508, .492, 0.06, 0.051, .0064, .76, .13, .013, .025, .18, .6, 0.006, 0.017, 0.878, 0.059, 0.022, 0.019, 0.045, 0.022, .228, 0.007, .208, 0.005, 0.007, 0.009, 0.008, 0.019, 0.016, NA, 0.004, 0.015, 0.254+0.147+0.065, 0.001515152),
      #      `2019` = c(.508, .492, 0.06, 0.051, .0064, .76, .13, .013, .025, .18, .6, 0.006, 0.017, 0.878, 0.059, 0.022, 0.019, 0.045, 0.022, .228, 0.007, .208, 0.005, 0.007, 0.009, 0.008, 0.019, 0.016, NA, 0.004, 0.015, 0.254+0.147+0.065, 0.001515152),
      #      `2020` = c(.508, .492, 0.06, 0.051, .0064, .76, .13, .013, .025, .18, .6, 0.006, 0.017, 0.878, 0.059, 0.022, 0.019, 0.045, 0.022, .228, 0.007, .208, 0.005, 0.007, 0.009, 0.008, 0.019, 0.016, NA, 0.004, 0.015, 0.254+0.147+0.065, 0.001515152)
      #      ) %>% 
      # pivot_longer(cols = matches("[0-9]{4}"), names_to = "year", values_to = "pct_population") %>% 
      # mutate(year = as.integer(year))

    DF_filtered =
      DF_filtered %>% 
      left_join(DF_pct_population, by = c("bias_motivation", "year")) %>% 
      mutate(value = round(((value / (total_population * pct_population)) * 100000), 2)) %>% 
      select(-pct_population) %>% 
      drop_na(value)
    
    # DF_filtered %>% distinct(year, bias_motivation, order) %>% pivot_wider(names_from = year, values_from = order)
  }
  
  # WIDE --------------------------------------------------------------------
  
  # Mean per group/year to order the plot
  DF_mean = DF_filtered %>% group_by(bias_motivation) %>% summarise(mean_value = mean(value, na.rm = TRUE)) %>% arrange(desc(mean_value))
  
  DF_wide =
    DF_filtered %>%
    select(bias_motivation, year, value, bias_supra) %>% 
    left_join(DF_mean, by = "bias_motivation") %>%
    
    # Arrange by value. IMPORTANT: arrange(year) must be last arrange
    arrange(!!!rlang::parse_exprs(arrange_by)) %>% select(-mean_value, -bias_supra) %>% 
    arrange(year) %>%
    
    pivot_wider(names_from = year, values_from = "value")
    
    
    
  
  # REVIEW: Insert BR to add line jump to long labels
  DF_wide = DF_wide %>% mutate(bias_motivation_short = bias_motivation)
    # insert_BR(DF_wide, name_label = "bias_motivation", where = 16) %>% 
    # REVIEW: This gets rid of the change made by insert_BR
    # Once we settle on a bias_motivation_short final value, get rid of this
    # mutate(bias_motivation_short = gsub(" <br> ", " ", bias_motivation_short))

  # Matrix ------------------------------------------------------------------
  
  DF_matrix = 
    DF_wide %>% 
    select(-bias_motivation, -bias_motivation_short) %>% 
    as.matrix() %>% 
    magrittr::set_rownames(DF_wide$bias_motivation_short) 
  
  
  return(DF_matrix)
  
}

