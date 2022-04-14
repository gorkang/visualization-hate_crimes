# Parameters --------------------------------------------------------------

# pid_target = 999


# Libraries ---------------------------------------------------------------

library(targets) 
library(tarchetypes) 


# Set options, load packages -----------------------------------------------

# Source all /R files
lapply(list.files("./R", full.names = TRUE, pattern = ".R$"), source)
options(pillar.sigfig = 5)

# Packages to load
main_packages = c("cli", "crayon", "furrr", "patchwork", "renv", "tarchetypes", "targets", "testthat")
data_preparation_packages = c("dplyr", "forcats", "here", "janitor", "purrr", "readr", "stringr", "tibble", "tidyr") #"safer", 
data_analysis_packages = c("broom", "broom.mixed", "emmeans", "gmodels", "gt", "gtsummary", "irr", "lme4", "parameters", "performance", "psych", "sjPlot") #"report"
data_visualization_packages = c("DT", "ggalluvial", "ggridges", "plotly", "htmlwidgets")
non_declared_dependencies = c("qs", "visNetwork", "webshot", "performance", "shinyWidgets")
extra_packages = c("shrtcts")
packages_to_load = c(main_packages, data_preparation_packages, data_analysis_packages, data_visualization_packages, non_declared_dependencies, extra_packages)

# target options (packages, errors...)
tar_option_set(packages = packages_to_load, # Load packages for all targets
               workspace_on_error = TRUE) # Needed to load workspace on error to debug


# Make sure tests run always
# if (file.exists("_targets/objects/TESTS") == TRUE ) targets::tar_invalidate(matches("TESTS"))



# Define targets -------------------------------------------------------------

targets <- list(
  
  # SETUP
  tar_target(SETUP, setup_project()),
  
  # GENERAL DATA PREPARATION
  tar_target(UNZIP_FBI, unzip_FBI(zip_folder = "data_raw/FBI/", output_folder = "data/FBI/", SETUP = SETUP)),
  
  
  # Diccionaries
  tar_target(DICC_FBI_t1, prepare_DICC_FBI_t1()),
  
  # Data preparation
  tar_target(DF_FBI_t1, prepare_data_FBI_t1(folder = "data/FBI/", diccionary = DICC_FBI_t1, UNZIPPED = UNZIP_FBI)),
  
  # Surface plots Data preparation
    # DF_FBI_t1 %>% distinct(bias_supra)
  tar_target(DF_FBI_t1_surface_sexual, prepare_data_FBI_t1_surface(DF = DF_FBI_t1, supra_sub = "sub", filter_bias_supra = "Sexual Orientation:")),
  tar_target(DF_FBI_t1_surface_religion, prepare_data_FBI_t1_surface(DF = DF_FBI_t1, supra_sub = "sub", filter_bias_supra = "Religion:")),
  tar_target(DF_FBI_t1_surface_gender, prepare_data_FBI_t1_surface(DF = DF_FBI_t1, supra_sub = "sub", filter_bias_supra = "Gender:")),
  tar_target(DF_FBI_t1_surface_GenderIdentity, prepare_data_FBI_t1_surface(DF = DF_FBI_t1, supra_sub = "sub", filter_bias_supra = "Gender Identity:")),
  tar_target(DF_FBI_t1_surface_race, prepare_data_FBI_t1_surface(DF = DF_FBI_t1, supra_sub = "sub", filter_bias_supra = "Race/Ethnicity/Ancestry:")),
  
  # Surface plots
  tar_target(PLOTS_FBI_t1_surface_sexual, plot_FBI_t1_surface(DF = DF_FBI_t1_surface_sexual, output_suffix = "SexualOrientation")),
  tar_target(PLOTS_FBI_t1_surface_religion, plot_FBI_t1_surface(DF = DF_FBI_t1_surface_religion, output_suffix = "Religion")),
  tar_target(PLOTS_FBI_t1_surface_gender, plot_FBI_t1_surface(DF = DF_FBI_t1_surface_gender, output_suffix = "Gender")),
  tar_target(PLOTS_FBI_t1_surface_GenderIdentity, plot_FBI_t1_surface(DF = DF_FBI_t1_surface_GenderIdentity, output_suffix = "GenderIdentity")),
  tar_target(PLOTS_FBI_t1_surface_race, plot_FBI_t1_surface(DF = DF_FBI_t1_surface_race, output_suffix = "Race")),
  
  
  # PLOTS
  tar_target(PLOTS_FBI_t1_lines, plot_FBI_t1_lines(DF = DF_FBI_t1)),
  tar_target(PLOTS_FBI_t1_density, plot_FBI_t1_density(DF = DF_FBI_t1))
  
)


# Declare pipeline --------------------------------------------------------

targets
