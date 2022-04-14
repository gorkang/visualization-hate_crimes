plot_FBI_t1_surface <- function(DF, output_suffix) {
  
   # PLOT --------------------------------------------------------------------
  
  # https://plotly-r.com/index.html
  # https://plotly.com/r/reference/#surface
  PLOT = plotly::plot_ly(
    z = ~ (DF), # OR t(mdf)
    x = ~ as.Date(colnames(DF), format = "%Y"),
    y = ~ rownames(DF),
    type = "surface",
    colorbar = list(title = "Incidents", ticks = "outside"),
    colorscale = "Viridis", # Blackbody,Bluered,Blues,Cividis,Earth,Electric,Greens,Greys,Hot,Jet,Picnic,Portland,Rainbow,RdBu,Reds,Viridis,YlGnBu,YlOrRd.
    showlegend = TRUE,
    showscale = FALSE, # legend dissapears
    connectgaps = FALSE, # REVIEW: FILLS NA with 0
    opacity = 1,
    
    # ADD 3D MESH
    # https://plotly.com/r/reference/#surface-hidesurface
    contours = list(
      x = list(show = TRUE),
      y = list(show = TRUE)
    ),
    hidesurface = FALSE
    # text = "in hover" # Add text in hover
  ) %>% 
    plotly::layout(
      title = paste0('FBI table 1: ', output_suffix),
      scene = list(
        autorange = FALSE,
        aspectmode = 'manual',
        
        # https://plotly.com/r/reference/layout/xaxis/#layout-xaxis
        xaxis = list(title = "date", nticks = 14, autorange = "reversed"),
        yaxis = list(title = "", nticks = 37), # bias motivations # Some labels not visible
        zaxis = list(title = "incidents"),
        aspectratio = list(x = 1, y = 2, z = 1),
        
        # https://plotly.com/r/reference/#layout-scene-camera
        camera = list(
          eye = list(x = 2, y = 1, z = 1),
          # eye = list(x = 1, y = 1, z = 0),
          center = list(x = 0.5, y = 0.2, z = 0),
          up = list(x = 0, y = 0, z = 1))
      )
    )
  
  PLOT
  
  
  # Export images --------------------------------------
  
  # with kaleido (python) 
  # install.packages('reticulate')
  # reticulate::install_miniconda()
  # reticulate::conda_install('r-reticulate', 'python-kaleido')
  # reticulate::conda_install('r-reticulate', 'plotly', channel = 'plotly')
  # reticulate::use_miniconda('r-reticulate')
  # kaleido(PLOT, "dev/PLOT.svg")
  
  # Save png
  plotly::save_image(PLOT, paste0("outputs/PLOTs/plot_FBI_t1_surface_", output_suffix, ".png"), width = 1600, height = 900)
  
  # Save interactive html
  htmlwidgets::saveWidget(PLOT, paste0("outputs/PLOTs/plot_FBI_t1_surface_", output_suffix, ".html"), selfcontained = TRUE, libdir = "lib")
  
  return(PLOT)
  
}