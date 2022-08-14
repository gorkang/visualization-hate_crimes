# See: https://crime-data-explorer.app.cloud.gov/pages/downloads

# targets::tar_renv()

targets::tar_destroy(ask = FALSE)

targets::tar_visnetwork(targets_only = TRUE, label = "time")

targets::tar_make()
