prepare_DICC_FBI_t2 <- function() {
  
  # DF_FBI_t2_raw %>% drop_na(incidents) %>%  filter(!grepl(":", offense_type)) %>% distinct(offense_type) %>% pull(offense_type) %>% datapasta::vector_construct()
  # targets::tar_load_globals()

  DF_supra = tibble(
    offense_supra = c("Crimes against persons:",
                   "Crimes against property:",
                   "Crimes against society:",
                   "Crimes against society:",
                   "Total"),
    offense_type = c("Crimes against persons:",
                     "Crimes against property:",
                     "Crimes against society5",
                     "Crimes against society",
                     "Total"))
  
 
  DF_persons = tibble(
    offense_supra = c("Crimes against property:"),
    offense_type = c("Murder and nonnegligent manslaughter",
      "Forcible rape",
      "Aggravated assault",
      "Simple assault",
      "Intimidation",
      
      # CLEAN UP
      "Rape (revised definition)4",
      "Rape (legacy definition)5",
      "Rape4",
      
      "Rape",
      "Human Trafficking, Commercial Sex Acts",
      "Other5",
      "Human trafficking, commercial sex acts"
    ))
  
  DF_property = tibble(
    offense_supra = c("Crimes against property:"),
    offense_type = c("Robbery",
                     "Burglary",
                     "Larceny-theft",
                     "Motor vehicle theft",
                     "Arson",
                     "Destruction/damage/vandalism"
    ))
  
  
  
  
  
  DICCIONARY = DF_supra %>% 
    bind_rows(DF_property) %>% 
    bind_rows(DF_persons)
  
  return(DICCIONARY)
  
}