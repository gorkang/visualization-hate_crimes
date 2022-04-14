prepare_data_FBI_t1 <- function(folder, diccionary, UNZIPPED) {
  
  # folder = "data/FBI/"
  
  all_files = list.files(folder, pattern = "xls", recursive = TRUE, full.names = TRUE) %>% as_tibble() %>% 
    filter(grepl("table1\\.|Table1\\.|Table 1\\.|Table_1_|Table 1-", value))
  
  files_table1 = tibble(order = as.character(1:5), year = 2006:2010, file = all_files$value[1:5])
  files_table2 = tibble(order = as.character(1:9), year = 2011:2019, file = all_files$value[6:14])
  
  DF1 = map_df(files_table1$file, readxl::read_excel, skip = 2, .id = "order") %>% left_join(files_table1, by = "order")
  DF2 = map_df(files_table2$file, readxl::read_excel, skip = 3, .id = "order") %>% left_join(files_table2, by = "order")
  
  
  DF1_clean = DF1 %>% janitor::remove_empty(which = "cols") %>% janitor::clean_names() %>% select(-known_offenders2_2) %>% rename(victims = victims1, known_offenders = known_offenders2)
  DF2_clean = DF2 %>% janitor::remove_empty(which = "cols") %>% janitor::clean_names() %>% rename(victims = victims1, known_offenders = known_offenders2)
  
  DF = DF1_clean %>% bind_rows(DF2_clean)
  
  DF_long = DF %>% pivot_longer(cols = incidents:known_offenders) %>% drop_na(value)
  
  # DF_long %>% distinct(year, bias_motivation, order) %>%  pivot_wider(names_from = year, values_from = order) %>% View
  
  # DF_long %>% 
  #   filter(bias_motivation %in% c("Ethnicity/National Origin:", "Race:")) %>% 
  #   filter(name == "incidents") %>% 
  #   select(bias_motivation, year, value) %>% 
  #   pivot_wider(names_from = year, values_from = value)
  
  DF_long_clean = DF_long  %>% 
    mutate(
      year = as.integer(year),
      bias_motivation = gsub("Anti-", "", bias_motivation)
    ) %>% 
    
    
    # IMPORTANT: REVIEW THIS DECISIONS
    
    # Ethnicity/National Origin: llega hasta 2014
    # Race/Ethnicity/Ancestry: registro completo
    
    # Asian/Pacific Islander se divide en Asian + Native Hawaiian/Other Pacific Islander en 2013?
    
    
    
  mutate(bias_motivation = 
           case_when(
             
             grepl("Male Homosexual", bias_motivation) ~ "Gay (Male)",
             grepl("Female Homosexual", bias_motivation) ~ "Lesbian",
             
             grepl("Homosexual", bias_motivation) ~ "Lesbian, Gay, Bisexual,/Transgender (Mixed Group)",
             
             
             
             grepl("Other Ethnicity/National Origin", bias_motivation) ~ "Other Race/Ethnicity/Ancestry",
             grepl("Not Hispanic/Latino3", bias_motivation) ~ "Other Race/Ethnicity/Ancestry",
             
             
             # "Race/Ethnicity/Ancestry:" is the final category, but there is overlap. 
             # TODO: Should use subcategories and 2019 classification to calculate totals per category.
             
             grepl("Ethnicity:", bias_motivation) ~ "Ethnicity:", #Ethnicity/National Origin:
             grepl("Ethnicity/National Origin:", bias_motivation) ~ "Ethnicity:",
             grepl("Race:", bias_motivation) ~ "Race/Ethnicity/Ancestry:",
             
             
             grepl("American Indian/Alaskan Native", bias_motivation) ~ "American Indian/Alaska Native",
             grepl("Black or African American", bias_motivation) ~ "African American",
             grepl("^Black$", bias_motivation) ~ "African American",
             grepl("^Hispanic$", bias_motivation) ~ "Hispanic/Latino",
             grepl("^Islamic$", bias_motivation) ~ gsub("Islamic", "Islamic (Muslim)", bias_motivation),
             
             # Simplificaciones
             grepl("Single-Bias Incidents", bias_motivation) ~ "Single-Bias",
             grepl("Multiple-Bias Incidents", bias_motivation) ~ "Multiple-Bias",
             grepl(" or ", bias_motivation) ~ gsub(" or ", "/", bias_motivation),
             TRUE ~ bias_motivation
           )) %>% 
    
    # Join DICCIONARY to get supra categories
    left_join(diccionary, by = "bias_motivation")
  
  
  # DF_long_clean %>% distinct(year, bias_motivation, order) %>%  pivot_wider(names_from = year, values_from = order) %>% View
  
  

  # WRITE -------------------------------------------------------------------

  DF_long_clean %>% write_csv2("outputs/DFs/FBI_table1_DF_long_clean.csv")
  
  

  # CHECK -------------------------------------------------------------------

  # SOMETHING WRONG: should be max 1 per category
  DF_CHECK = DF_long_clean %>% count(bias_motivation, year, name) %>% filter(n > 1)
  if (nrow(DF_CHECK) > 0) cli::cli_abort("SHOULD be only 1 per category! Review DF_long_clean")
  
  
  return(DF_long_clean)
  
}