setup_project <- function() {
  
  # Save surface plots with kaleido (python) 
    # install.packages('reticulate')
    # reticulate::install_miniconda()
    # reticulate::conda_install('r-reticulate', 'python-kaleido')
    # reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
    # reticulate::use_miniconda('r-reticulate')
    # SAVE # kaleido(PLOT, "dev/PLOT.svg")
  
  # Check all necessary packages installed --------------------------------
  
  if (file.exists("_targets_packages.R")) {
    packages_renv = gsub("library\\(|\\)", "", readLines("_targets_packages.R")[-1])
    # Asks user before installing all packages missing (if any)
    rlang::check_installed(packages_renv, reason = "for project to work") 
  } else {
    cli::cli_abort("RUN `targets::tar_renv()` in the console before proceeding")
  }
  
  # Make sure all the necessary folders exist -----------------------------
  
  necessary_folders = c("data/FBI", "data/NCVS", "outputs/DFs", "outputs/PLOTs/", "outputs/PLOTs/surface")
  
  if (all(necessary_folders %in% dir(recursive = TRUE, include.dirs = TRUE, all.files = TRUE))) {
    
    cli::cli_alert_success("All the necessary folders are present\n")
    
  } else {
    
    cli::cli_alert_warning("Creating necessary folders: \n")
    cli::cli_li(paste(necessary_folders, collapse = ", "), "\n")
    invisible(purrr::map(necessary_folders, dir.create, recursive = TRUE, showWarnings = FALSE))
    # system("chmod 700 -R .vault/")
    
  }
  
  output_message = "SETUP OK"
  
  return(output_message)
  
}