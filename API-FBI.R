# USE API to get FBI crime data directly
  # Get aPI: https://api.data.gov/signup/
  # API = "F6SGaYp7FageG7ijhhcF1QjKEb74dYhKlyPQFRz3"

# How to build api calls: # https://crime-data-explorer.fr.cloud.gov/pages/docApi

# For hate crimes:  

# GET /api/hatecrime/national/{offense}/{variable} National level Hate Crime Count Endpoint
  # ofense = all
  # variable = BIAS

URL = "https://api.usa.gov/crime/fbi/sapi/api/hatecrime/national/all/BIAS?API_KEY=F6SGaYp7FageG7ijhhcF1QjKEb74dYhKlyPQFRz3"
DF_api = jsonlite::fromJSON(URL)

DF_api_clean = DF_api$data %>% as_tibble() %>% arrange(data_year) %>% 
  select(-month_num) %>% 
  rename(year = data_year) %>% 
  select(key, year, value) %>% 
  # mutate(source = "api") %>% 
  mutate(key = gsub("Anti-", "", key))
                # %>% pivot_wider(names_from = "data_year", values_from = "value") 

DF_tables_clean = DF_FBI_t1_surface_ALL %>% as_tibble(rownames = "key") %>% pivot_longer(`2006`:last_col(), names_to = "year") %>% 
  # mutate(source = "tables") %>% 
  mutate(year = as.numeric(year))

DF_api_clean %>% full_join(DF_tables_clean, by = c("key", "year"), suffix = c("_API", "_TABLE")) %>% 
  mutate(DIFF = value_API - value_TABLE) %>% View
