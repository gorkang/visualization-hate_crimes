plot_FBI_t1_surface <- function(DF, output_suffix, table) {
  
  # DEBUG
  # DF = DF_matrix
  # output_suffix = "ALL"
  # table = 1

  
  
  # PLOT --------------------------------------------------------------------
  
  # https://plotly-r.com/index.html
  # https://plotly.com/r/reference/#surface
  
  # y_axis_labels = rownames(DF) #gsub(" <br> ", " ", rownames(DF))
  y_axis_labels = gsub(" <br> ", " ", rownames(DF))
  
  # Fill to make all same size. Does not work
  # m_char <- max(nchar(y_axis_labels)) 
  # y_axis_labels = stringr::str_pad(y_axis_labels, m_char, side = "right", pad = "_") 
  
  # y_axis_labels = 
  #   tibble(bias_motivation = stringr::str_sub(gsub(" <br> ", " ", rownames(DF)))) %>% 
  #   mutate(bias_motivation_short =
  #            case_when(
  #              str_length(bias_motivation) > 16 ~  paste0(stringr::str_sub(bias_motivation, end = 16), "..."),
  #              TRUE ~ bias_motivation
  #              )) %>% 
  #   pull(bias_motivation_short)
  
  
  
  PLOT = plotly::plot_ly(
    z = ~ (DF), # OR t(mdf)
    x = ~ as.Date(colnames(DF), format = "%Y"),
    y = ~ y_axis_labels, #rownames(DF),
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
      title = paste0('FBI table ', table, ': ', output_suffix),
      scene = list(
        aspectmode = 'manual',
        
        # https://plotly.com/r/reference/layout/xaxis/#layout-xaxis
        xaxis = list(title = "", nticks = 14, autorange = "reversed"), #date
        yaxis = list(title = "", 
                     # domain = c(0.25, 0.75),
                     nticks = 37, 
                     # constrain = "range", 
                     constraintoward = "left",
                     # dtick = 2 # Show 1 every 2 ticks
                     # gridwidth = 1 # Width of grid
                     autorange = TRUE,
                     automargin = FALSE,
                     
                     # tickangle = -60, tickfont = list(family='Rockwell', color='crimson', size=12),
                     # tickwidth = 1,
                     
                     
                     # color = "#888888", # colors lines and text
                     tickfont = list(color = "#999999", size = 12) # formats just font
                     
                     
                     ), # bias motivations # Some labels not visible
        zaxis = list(title = "incidents", 
                     separatethousands = TRUE,
                     ticks = "outside", 
                     tickwidth = 2, 
                     tickcolor = 'crimson', 
                     ticklen = 10, 
                     col = 1), 
        
        # Aspect ratio of plot
        aspectratio = list(x = 1, y = 2, z = 1),
        
        # Initial position of camera
        # https://plotly.com/r/reference/#layout-scene-camera
        camera = list(
          eye = list(x = 2, y = 1, z = 1),
          # eye = list(x = 1, y = 1, z = 0),
          center = list(x = 0.5, y = 0.2, z = 0),
          up = list(x = 0, y = 0, z = 1))
      )
    ) 
  
    # plotly::style(text = c("giraffes", " orange orangutans", "pigs"), textposition = "auto", insidetextanchor="start")
  
  
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
  plotly::save_image(PLOT, paste0("outputs/PLOTs/surface/plot_FBI_t", table, "_surface_", output_suffix, ".png"), width = 1600, height = 900)
  
  # Save interactive html
  htmlwidgets::saveWidget(PLOT, paste0("outputs/PLOTs/surface/plot_FBI_t", table, "_surface_", output_suffix, ".html"), selfcontained = TRUE, libdir = "lib")
  
  return(PLOT)
  
}
