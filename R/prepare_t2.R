prepare_t2 <- function(DF, diccionary) {
  
  # DEBUG
  # DF = DF_FBI_t2_raw
  # diccionary = DICC_FBI_t2
  
  DF_long = DF %>% pivot_longer(cols = incidents:known_offenders) %>% drop_na(value) %>% 
    mutate(offense_type = gsub("[[:digit:]]$", "", offense_type))
  
  # DF_long %>% distinct(offense_type) %>% head(20)
  
  DF_long_clean = DF_long  %>% 
    # mutate(
    #   year = as.integer(year),
    #   offense_type = gsub("Anti-", "", offense_type)
    # ) %>% 
    
    
    mutate(offense_type = 
             case_when(
               # TABLE 2
               grepl("Rape \\(revised definition\\)4", offense_type) ~ "Rape",
               grepl("Rape \\(revised definition\\)5", offense_type) ~ "Rape",
               grepl("Rape \\(revised definition\\)", offense_type) ~ "Rape",
               grepl("Rape4", offense_type) ~ "Rape",
               
               # TABLE 1
               grepl("Male Homosexual", offense_type) ~ "Gay (Male)",
               grepl("Female Homosexual", offense_type) ~ "Lesbian",
               grepl("Homosexual", offense_type) ~ "Lesbian, Gay, Bisexual,/Transgender (Mixed Group)",
               grepl("Other Ethnicity/National Origin", offense_type) ~ "Other Race/Ethnicity/Ancestry",
               grepl("Not Hispanic/Latino3", offense_type) ~ "Other Race/Ethnicity/Ancestry",
               # "Race/Ethnicity/Ancestry:" is the final category, but there is overlap. 
               # TODO: Should use subcategories and 2019 classification to calculate totals per category.
               grepl("Ethnicity:", offense_type) ~ "Ethnicity:", #Ethnicity/National Origin:
               grepl("Ethnicity/National Origin:", offense_type) ~ "Ethnicity:",
               grepl("Race:", offense_type) ~ "Race/Ethnicity/Ancestry:",
               grepl("American Indian/Alaskan Native", offense_type) ~ "American Indian/Alaska Native",
               grepl("Black or African American", offense_type) ~ "African American",
               grepl("^Black$", offense_type) ~ "African American",
               grepl("^Hispanic$", offense_type) ~ "Hispanic/Latino",
               grepl("^Islamic$", offense_type) ~ gsub("Islamic", "Islamic (Muslim)", offense_type),
               # Simplificaciones
               grepl("Single-Bias Incidents", offense_type) ~ "Single-Bias:",
               grepl("Multiple-Bias Incidents", offense_type) ~ "Multiple-Bias:",
               grepl(" or ", offense_type) ~ gsub(" or ", "/", offense_type),
               TRUE ~ offense_type
             )) %>% 
    
    # Join DICCIONARY to get supra categories
    left_join(diccionary, by = "offense_type") %>%
    select(-skips)
  
  
  
  # WRITE -------------------------------------------------------------------
  
  DF_long_clean %>% write_csv2("outputs/DFs/FBI_table2_DF_long_clean.csv")
  
  
  
  # CHECK -------------------------------------------------------------------
  
  # SOMETHING WRONG: should be max 1 per category
  DF_CHECK = DF_long_clean %>% 
    filter(offense_type != "Other") %>% # TODO : Ponerle prefijo a los Other en funcion de la categoria a la que pertenecen
    count(offense_type, year, name) %>% filter(n > 1)
  if (nrow(DF_CHECK) > 0) cli::cli_abort("SHOULD be only 1 per category! Review DF_long_clean")
  
  
  return(DF_long_clean)
  
}