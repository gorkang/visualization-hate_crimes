prepare_data_NCVS <- function(file_name, skip_n, UNZIPPED) {
  
  # DETECT ENCODING OF FILE
  # file_path = "data/NCVS/0519/hcv0519t01.csv"
  # ENC = readr::guess_encoding(file_path, n_max = 1000)
  # readr::read_csv(file_path, skip = 11, locale = locale(encoding = ENC$encoding[1]))
  
  
  file_path = paste0("data/NCVS/0519/", file_name)
  DF_raw = suppressMessages(readr::read_csv(file_path, skip = skip_n, locale = locale(encoding = "windows-1252")))
  

  # Specific transformations depending on the file --------------------------

  if (file_name == "hcv0519t01.csv") {
  
    delete_columns = names(DF_raw)[grepl("^\\.\\.\\.[0-9]{1,2}$", names(DF_raw))]
    DF_clean = DF_raw %>% select(-all_of(delete_columns))
    
    # Clean columns with symbols. 
    # TODO: Some of the symbols convey important messages
    clean_columns = gsub("^(.*)\\.\\.\\.[0-9]{1,2}$", "\\1", names(DF_clean))
  
    suffix_total = rep("Total_", each = 3)
    suffix_others = rep(c("Violent_", "Property_"), each = 4)
    suffixes = c(suffix_total, suffix_others)
    final_names = c(clean_columns[1], paste0(suffixes, clean_columns[-1], sep = ",")) %>% janitor::make_clean_names()%>% gsub("_b|_c", "", .)
    
    names(DF_clean) = final_names
    
    # Remove * from years and keep only rows with years
    DF_long = DF_clean %>% 
      mutate(year = gsub("\\*", "", year)) %>% 
      filter(grepl("^[0-9]{4}$", year)) %>% 
      pivot_longer(total_total:property_percent) %>% 
      separate(name, into = c("supra", "sub"), extra = "merge", sep = "_")
    
  } else {
    cli::cli_abort("File '{file_name}' not recognized. CHECK prepare_data_NCVS()")
  }
  
  return(DF_long)
  
}



