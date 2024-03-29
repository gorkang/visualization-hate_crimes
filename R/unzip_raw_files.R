unzip_raw_files <- function(zip_folder = "data_raw/FBI/", output_folder = "data/FBI/", SETUP) {

  # List zips
  zips = list.files(zip_folder, recursive = TRUE, pattern = "zip", full.names = TRUE)
  
  if (any(grepl("\\(2\\)", zips))) cli::cli_abort("(2) files in the data_raw/")
  
  # Unzip in new folders
  zips_folders <- paste0(output_folder, unlist(regmatches(zips, gregexpr("[[:digit:]]+", zips))))
  1:length(zips_folders) %>% walk(~ dir.create(zips_folders[.x], recursive = TRUE, showWarnings = FALSE))
  1:length(zips) %>% walk(~ unzip(zipfile = zips[.x], exdir = zips_folders[.x]))
  
  message_output = paste0("RAW files unziped in: ", paste(zips_folders, collapse = ", "))
  return(message_output)
  
}