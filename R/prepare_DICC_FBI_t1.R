prepare_DICC_FBI_t1 <- function() {

  DF_supra = tibble(
    bias_supra = c("Total", 
                   "Single-Bias",
                   "Multiple-Bias",
                   "Race/Ethnicity/Ancestry:",
                   "Religion:",
                   "Sexual Orientation:",
                   "Disability:",
                   "Gender:",
                   "Gender Identity:"),
    bias_motivation = c("Total", 
                        "Single-Bias",
                        "Multiple-Bias",
                        "Race/Ethnicity/Ancestry:",
                        "Religion:",
                        "Sexual Orientation:",
                        "Disability:",
                        "Gender:",
                        "Gender Identity:"))
  
  DF_race = tibble(
    bias_supra = c("Race/Ethnicity/Ancestry:"),
    bias_motivation = c(
      "White",
      "African American",
      "Black/African American",
      "American Indian/Alaska Native",
      "Asian",
      "Native Hawaiian/Other Pacific Islander",
      "Multiple Races, Group",
      "Arab",
      "Hispanic/Latino",
      "Other Race/Ethnicity/Ancestry"
    )
  )
  
  
  DF_religion = tibble(
    bias_supra = c("Religion:"),
    bias_motivation = c(
      "Jewish",
      "Catholic",
      "Protestant",
      "Islamic (Muslim)",
      "Other Religion",
      "Multiple Religions, Group",
      "Mormon",
      "Jehovah's Witness",
      "Eastern Orthodox (Russian, Greek, Other)",
      "Other Christian",
      "Buddhist",
      "Hindu",
      "Sikh",
      "Atheism/Agnosticism/etc."
    )
  )
  
  
  
  DF_sexual_orientation = tibble(
    bias_supra = c("Sexual Orientation:"),
    bias_motivation = c(
      "Gay (Male)",
      "Lesbian",
      "Lesbian, Gay, Bisexual,/Transgender (Mixed Group)",
      "Heterosexual",
      "Bisexual"
    )
  )
  
  
  DF_disability = tibble(
    bias_supra = c("Disability:"),
    bias_motivation = c(
      "Physical",
      "Mental"
    )
  )
  DF_gender = tibble(
    bias_supra = c("Gender:"),
    bias_motivation = c(
      "Male",
      "Female"
    )
  )
  DF_gender_identity = tibble(
    bias_supra = c("Gender Identity:"),
    bias_motivation = c(
      "Transgender",
      "Gender Non-Conforming",
      "Multiple-Bias Incidents3"
    )
  )
  
  
  
  
  
  DICCIONARY = DF_supra %>% 
    bind_rows(DF_race) %>% 
    bind_rows(DF_religion) %>% 
    bind_rows(DF_sexual_orientation) %>% 
    bind_rows(DF_disability) %>% 
    bind_rows(DF_gender) %>% 
    bind_rows(DF_gender_identity)
  
  return(DICCIONARY)

}