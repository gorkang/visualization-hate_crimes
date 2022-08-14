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
data_analysis_packages = c("broom", "broom.mixed", "emmeans", "gt", "gtsummary", "irr", "lme4", "parameters", "performance", "psych", "sjPlot") #"report"
data_visualization_packages = c("DT", "ggalluvial", "ggridges", "htmlwidgets", "plotly")
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
  tar_target(UNZIP_FBI, unzip_raw_files(zip_folder = "data_raw/FBI/", output_folder = "data/FBI/", SETUP = SETUP)),
  tar_target(UNZIP_NCVS, unzip_raw_files(zip_folder = "data_raw/NCVS/", output_folder = "data/NCVS/", SETUP = SETUP)),
  
  
  ## FBI READ TABLES --------------------------------------------------------
  
  # Diccionaries
  tar_target(DICC_FBI_t1, prepare_DICC_FBI_t1()),
  tar_target(DICC_FBI_t2, prepare_DICC_FBI_t2()),
  
  # Data preparation
  # tar_target(DF_FBI_t1, prepare_data_FBI_t1(folder = "data/FBI/", diccionary = DICC_FBI_t1, UNZIPPED = UNZIP_FBI)),
  tar_target(DF_FBI_t1_raw, prepare_data_FBI(folder = "data/FBI/", table = 1, keyword = "Bias motivation", UNZIPPED = UNZIP_FBI)),
  tar_target(DF_FBI_t2_raw, prepare_data_FBI(folder = "data/FBI/", table = 2, keyword = "Offense type", UNZIPPED = UNZIP_FBI)),
  
  tar_target(DF_FBI_t1_API_raw, get_API_data(API_key)), # Set API_key in .Rprofile
             

  # tar_target(DF_FBI_t1_table, prepare_t1(DF_table = DF_FBI_t1_raw, DF_api = DF_FBI_t1_API_raw, diccionary = DICC_FBI_t1,
  #                                  method = "table")),
  
  tar_target(DF_FBI_t1, prepare_t1(DF_table = DF_FBI_t1_raw, DF_api = DF_FBI_t1_API_raw, diccionary = DICC_FBI_t1, 
                                   method = "api")),

  
  tar_target(DF_FBI_t2, prepare_t2(DF = DF_FBI_t2_raw, diccionary = DICC_FBI_t2)),
  
  
  
  ## NCVS READ TABLES -------------------------------------------------------
  tar_target(DF_NCVS_t01_raw, prepare_data_NCVS(file_name = "hcv0519t01.csv", skip_n = 12, UNZIPPED = UNZIP_NCVS)),
  
  

  # PLOTS ---------------------------------------------------------------


  ## FBI ---------------------------------------------------------------------

  # SHOULD USE tarchetypes::tar_map() or similar to create all programatically
  # DF_FBI_t1 %>% distinct(bias_supra)
  
  
  # Surface plots Data preparation
  tar_target(DF_FBI_t1_surface_ALL, prepare_data_FBI_t1_surface(DF = DF_FBI_t1, supra_sub = "sub", filter_bias_supra = "*", variable = "victims")),
  tar_target(DF_FBI_t1_surface_ALL_supra, prepare_data_FBI_t1_surface(DF = DF_FBI_t1, supra_sub = "supra", filter_bias_supra = "*", variable = "victims")),
  tar_target(DF_FBI_t1_surface_sexual, prepare_data_FBI_t1_surface(DF = DF_FBI_t1, supra_sub = "sub", filter_bias_supra = "Sexual Orientation:", variable = "victims")),
  tar_target(DF_FBI_t1_surface_religion, prepare_data_FBI_t1_surface(DF = DF_FBI_t1, supra_sub = "sub", filter_bias_supra = "Religion:", variable = "victims")),
  tar_target(DF_FBI_t1_surface_gender, prepare_data_FBI_t1_surface(DF = DF_FBI_t1, supra_sub = "sub", filter_bias_supra = "Gender:", variable = "victims")),
  tar_target(DF_FBI_t1_surface_GenderIdentity, prepare_data_FBI_t1_surface(DF = DF_FBI_t1, supra_sub = "sub", filter_bias_supra = "Gender Identity:", variable = "victims")),
  tar_target(DF_FBI_t1_surface_race, prepare_data_FBI_t1_surface(DF = DF_FBI_t1, supra_sub = "sub", filter_bias_supra = "Race/Ethnicity/Ancestry:", variable = "victims")),
  
  tar_target(DF_FBI_t2_surface_ALL, prepare_data_FBI_t2_surface(DF = DF_FBI_t2, supra_sub = "sub", filter_offense_supra = "*")),
  
  
  # Surface plots
  # table 1
  tar_target(PLOTS_FBI_t1_surface_ALL, plot_FBI_t1_surface(DF = DF_FBI_t1_surface_ALL, output_suffix = "ALL", table = 1)),
  tar_target(PLOTS_FBI_t1_surface_ALL_supra, plot_FBI_t1_surface(DF = DF_FBI_t1_surface_ALL_supra, output_suffix = "ALL_supra", table = 1)),
  tar_target(PLOTS_FBI_t1_surface_sexual, plot_FBI_t1_surface(DF = DF_FBI_t1_surface_sexual, output_suffix = "SexualOrientation", table = 1)),
  tar_target(PLOTS_FBI_t1_surface_religion, plot_FBI_t1_surface(DF = DF_FBI_t1_surface_religion, output_suffix = "Religion", table = 1)),
  tar_target(PLOTS_FBI_t1_surface_gender, plot_FBI_t1_surface(DF = DF_FBI_t1_surface_gender, output_suffix = "Gender", table = 1)),
  tar_target(PLOTS_FBI_t1_surface_GenderIdentity, plot_FBI_t1_surface(DF = DF_FBI_t1_surface_GenderIdentity, output_suffix = "GenderIdentity", table = 1)),
  tar_target(PLOTS_FBI_t1_surface_race, plot_FBI_t1_surface(DF = DF_FBI_t1_surface_race, output_suffix = "Race", table = 1)),
  
  # table 2
  tar_target(PLOTS_FBI_t2_surface_ALL, plot_FBI_t1_surface(DF = DF_FBI_t2_surface_ALL, output_suffix = "ALL", table = 2)),
  
  # General PLOTS
  tar_target(PLOTS_FBI_t1_lines, plot_FBI_t1_lines(DF = DF_FBI_t1)),
  tar_target(PLOTS_FBI_t1_density, plot_FBI_t1_density(DF = DF_FBI_t1)),
  
  

  # NCVS --------------------------------------------------------------------
  
  tar_target(PLOTS_NCVS, plot_NCVS(DF = DF_NCVS_t01_raw))
  
)


# Declare pipeline --------------------------------------------------------

targets
