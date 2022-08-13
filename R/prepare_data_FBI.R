 # <- function(folder, UNZIPPED, table) {

prepare_data_FBI <- function(folder = "data/FBI/", table, keyword, UNZIPPED) {
  
  
  # Get row where the keyword is to skip previous rows
  get_key_rows <- function(all_files, keyword) {
    # keyword = "sdf"
    KEY_ROWS = map_int(all_files$file,
            ~ {
              # .x = all_files$file[8]
              # cli::cli_alert(.x)
              DF1 = suppressMessages(readxl::read_excel(.x, col_names = "ONE", range = "A1:A5"))
              initial_row = which(DF1$ONE == keyword) # Try exact match
              # if fails, try regexp (allows to use two keywords separated by |)
              # If we find more than one keyword, we keep last()
              if (length(initial_row) == 0) initial_row = (which(grepl(keyword, DF1$ONE)))
              if (length(initial_row) == 0) cli::cli_abort("Keyword {keyword} not found in {.x}. First 5 rows are: `{DF1$ONE}`")
              initial_row
            }) - 1
  }


  clean_names_columns <- function(DF) {
    
    # .x = 13
    # DF = readxl::read_excel(all_files_skips$file[.x], skip = all_files_skips$skips[.x]) %>% janitor::remove_empty(which = "cols")
    # names(DF)
    
    # If there are unnamed columns
    if (any(grepl("x[0-9]{1}|\\.\\.\\.[0-9]{1}", names(DF)))) {
      
      columns_to_change = DF[1,] %>% janitor::remove_empty(which = "cols")
      oldnames = names(columns_to_change)
      newnames = as.character(columns_to_change[1,]) %>% gsub("[[:digit:]]$", "", .) # Use first row for names. Delete numbers at the end
      
      # Check for duplicate columns (e.g. Other1 in Table 4)
      duplicated_names = newnames[duplicated(newnames)]
      
      # If there is one duplicate, add "_dup
      if (length(duplicated_names) == 1) {
        newnames[which(newnames == duplicated_names)[2]] = paste0(duplicated_names, "_dup")
      } else if (length(duplicated_names) > 1) {
        cli::cli_abort("More than 1 duplicate column name")
      }
      
      DF_clean = DF %>% 
        rename_with(~ newnames[which(oldnames == .x)], .cols = oldnames) %>% 
        .[-1,] # Remove first row
      
    } else {
      DF_clean = DF
    }
    
    DF_clean %>% 
      janitor::clean_names(replace = c("\n" = "_")) %>% 
      janitor::remove_empty(which = "cols")
  }
  
    
    # folder = "data/FBI/"
    # table = 9
    # keyword = "Race/Ethnicity/Age|Known Offender's Race"
    # keyword = "Offense type"
    # keyword = "Bias motivation"
    
    tables_grep = glue::glue("table{table}\\.|Table{table}\\.|Table {table}\\.|Table_{table}_|Table {table}-")
    all_files = list.files(folder, pattern = "xls", recursive = TRUE, full.names = TRUE) %>% 
      as_tibble() %>% 
      filter(grepl(tables_grep, value)) %>% 
      mutate(order = as.character(1:nrow(.)), year = 2006:2020, file = value) %>% select(-value)
    
    # if (length(all_files$file) != 15) cli::cli_abort("We found {length(all_files$file)} files in {folder} looking for table {table}. Should be 15")
    
    skips = tibble(skips = get_key_rows(all_files, keyword = keyword))
    all_files_skips = all_files %>% bind_cols(skips)
      
    # .x = 1
    DF_out = 
      1:length(all_files_skips$file) %>% 
      map_df(~ 
               {
                 # cli::cli_alert(.x)
                 
                 # readxl::read_excel(all_files_skips$file[.x], skip = 0)
                 DF_temp = suppressMessages(readxl::read_excel(all_files_skips$file[.x], skip = all_files_skips$skips[.x])) %>% 
                   janitor::remove_empty(which = "cols") %>% 
                   clean_names_columns(.)
                 
                 # cli::cli_alert(str(DF_temp))
                 DF_temp
                },
                 .id = "order") %>% 
      left_join(all_files_skips, by = "order")
    
      
      

    # Renaming ----------------------------------------------------------------
 
      # If missing only two columns, most likely will be keyword column and total
      # if (("x1" %in% names(DF_out) & "x2" %in% names(DF_out)) & !"x3" %in% names(DF_out)) {
      #   names(DF_out) = gsub("^x1$", janitor::make_clean_names(keyword), names(DF_out))
      #   names(DF_out) = gsub("^x2$", "total", names(DF_out))
      # }
    
      names(DF_out) <- gsub("[[:digit:]]", "", names(DF_out))
      
      
      return(DF_out)
  } 
  
  # DF1 = read_all(folder = "data/FBI/", table = 1, keyword = "Bias motivation")
  # DF2 = read_all(folder = "data/FBI/", table = 2, keyword = "Offense type")
  # 
  # # En table 3 hay que leer los rownames de filas 2 y 3 y combinar. Ver colnames()
  # # Pasa cuando hay celdas anidadas... Ver table3: /home/emrys/gorkang@gmail.com/RESEARCH/PROYECTOS/2022-Noemi Pereda/visualization-hate_crimes/data/FBI/07/hatecrimetables/table3.xls
  # DF3 = read_all(folder = "data/FBI/", table = 3, keyword = "Offense type")
  # DF4 = read_all(folder = "data/FBI/", table = 4, keyword = "Bias motivation")
  # DF5 = read_all(folder = "data/FBI/", table = 5, keyword = "Bias motivation")
  # DF6 = read_all(folder = "data/FBI/", table = 6, keyword = "Offense type")
  # DF7 = read_all(folder = "data/FBI/", table = 7, keyword = "Bias motivation")
  # DF8 = read_all(folder = "data/FBI/", table = 8, keyword = "Bias motivation")
  # 
  # # Terrible
  # DF9 = read_all(folder = "data/FBI/", table = 9, keyword = "Race/Ethnicity/Age|Table 9") # Can be solved with last(), but causes issues when keyword not present. See DF10
  # 
  # DF10 = read_all(folder = "data/FBI/", table = 10, keyword = "Bias Motivation")

  
  # }








