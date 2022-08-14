get_API_data <- function(API_key) {
  
  
  # Get data using API ------------------------------------------------------
  
  URL = paste0("https://api.usa.gov/crime/fbi/sapi/api/hatecrime/national/all/BIAS?API_KEY=", API_key)
  DF_api = jsonlite::fromJSON(URL)
  
  # Adapt to tables format
  DF_api_clean = DF_api$data %>% as_tibble() %>% arrange(data_year) %>% 
    select(-month_num) %>% 
    rename(year = data_year,
           bias_motivation = key,
           victims = value) %>% 
    mutate(order = 1,
           file = "api",
           skips = NA,
           incidents = NA,
           offenses = NA, 
           known_offenders = NA
           ) %>% 
    select(order, bias_motivation, incidents, offenses, victims, known_offenders, year, file, skips) 
  
  return(DF_api_clean)
  
}
