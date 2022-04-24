prepare_t1 <- function(DF, diccionary) {
  
  # DEBUG
  # DF = DF_FBI_t1_raw
  # diccionary = DICC_FBI_t1
  # DF_long %>% distinct(bias_motivation) %>% View
  
  DF_long = DF %>% pivot_longer(cols = incidents:known_offenders) %>% drop_na(value)
  
  DF_long_clean = DF_long  %>% 
    mutate(
      year = as.integer(year),
      bias_motivation = gsub("Anti-", "", bias_motivation),
      bias_motivation = gsub("[[:digit:]]", "", bias_motivation)
    ) %>% 
    
    # REVIEW DECISIONS
    mutate(bias_motivation = 
             case_when(
               # Total
               # Single-Bias Incidents
               
              # Race/Ethnicity/Ancestry:
               grepl("American Indian/Alaskan Native", bias_motivation) ~ "American Indian/Alaska Native",
               grepl("Black or African American", bias_motivation) ~ "African American",
               grepl("^Black$", bias_motivation) ~ "African American",
               grepl("^Hispanic$", bias_motivation) ~ "Hispanic/Latino",
             
               grepl("Other Ethnicity/National Origin", bias_motivation) ~ "Other Race/Ethnicity/Ancestry",
               grepl("Not Hispanic or Latino", bias_motivation) ~ "Other Race/Ethnicity/Ancestry",
               
               
              # Religion:
              grepl("^Islamic$", bias_motivation) ~ gsub("Islamic", "Islamic (Muslim)", bias_motivation),
              
              
              # Sexual Orientation:
                # 2006, 2007... 
                grepl("^Homosexual$", bias_motivation) ~ "Lesbian, Gay, Bisexual,/Transgender (Mixed Group)",
                grepl("Male Homosexual", bias_motivation) ~ "Gay (Male)",
                grepl("Female Homosexual", bias_motivation) ~ "Lesbian",
                 
               
              # Disability:
                 # Physical
                 # Mental
               
              # Gender:
              # Gender Identity:
              # Multiple-Bias Incidents
               
               
               
               # "Race/Ethnicity/Ancestry:" is the final category, but there is overlap. 
               # TODO: Should use subcategories and 2019 classification to calculate totals per category.
               
               grepl("Ethnicity/National Origin:", bias_motivation) ~ "Ethnicity:",
               grepl("Race:", bias_motivation) ~ "Race/Ethnicity/Ancestry:",
               
               # Simplificaciones
               grepl("Single-Bias Incidents", bias_motivation) ~ "Single-Bias:",
               grepl("Multiple-Bias Incidents", bias_motivation) ~ "Multiple-Bias:",
               grepl(" or ", bias_motivation) ~ gsub(" or ", "/", bias_motivation),
               TRUE ~ bias_motivation
             )) %>% 
    
    # Join DICCIONARY to get supra categories
    left_join(diccionary, by = "bias_motivation") %>% 
    select(-skips) #%>% select(year, bias_motivation,bias_motivation2)
  
  
  
  # WRITE -------------------------------------------------------------------
  
  # app.R Reads this file
  DF_long_clean %>% write_csv2("outputs/DFs/FBI_table1_DF_long_clean.csv")
  
  
  
  # CHECK -------------------------------------------------------------------
  
  # SOMETHING WRONG: should be max 1 per category
  DF_CHECK = DF_long_clean %>% count(bias_motivation, year, name) %>% filter(n > 1)
  if (nrow(DF_CHECK) > 0) cli::cli_abort("SHOULD be only 1 per category! Review DF_long_clean")
  
  
  return(DF_long_clean)
  
}