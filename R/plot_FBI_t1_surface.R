plot_FBI_t1_surface <- function(DF, output_suffix, table) {
  
  # DEBUG
  # DF = DF_matrix
  # DF = DF_FBI_t1_surface_ALL
  # output_suffix = "ALL"
  # table = 1

  # DF = DF_FBI_t1_surface_gender
  # output_suffix = "ALL"
  # table = 1
  
  
  # PLOT --------------------------------------------------------------------
  
  # https://plotly-r.com/index.html
  # https://plotly.com/r/reference/#surface
  
  # Y axis labels are too LONG
  # - We can shorten, or add a br to make them two lines, ...
  
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
  
  
  spike_color = "#111111"
  highlight_color = "#41a7b3"
  hover_background = "rgba(255,255,255,0.75)" # OR "#ffffff"
  tick_labels_color = "#222222"
  show_spikes = FALSE # Straight lines. Not very useful if we have highlight

  # x_axis = as.Date(colnames(DF), format = "%Y")
  x_axis = colnames(DF)
  y_axis = y_axis_labels
  z_axis = DF # OR t(DF)
    
  n_ticks_x = length(x_axis)
  n_ticks_y = length(y_axis_labels)
  
  PLOT = plotly::plot_ly(
    x = ~ x_axis,
    y = ~ y_axis,
    z = ~ z_axis,
    type = "surface",
    # colorbar = list(title = "Incidents", ticks = "outside"),
    colorscale = "Viridis", # Blackbody,Bluered,Blues,Cividis,Earth,Electric,Greens,Greys,Hot,Jet,Picnic,Portland,Rainbow,RdBu,Reds,Viridis,YlGnBu,YlOrRd.
    showlegend = FALSE,
    showscale = FALSE, # legend dissapears
    connectgaps = FALSE, # FILLS NA with 0 # Freezes when overing over NA value
    opacity = 1, # Transparency of surface 0-1
    
    # Template for hover
    hovertemplate = paste(
      "<b>%{y}</b><br>",
      "Year: %{x:%Y}<br>",
      "Incidents: %{z}",
      "<extra></extra>"
    ),
    # hoverlabel = list(bgcolor = "rgba(255,255,255,0.75)"),
    hidesurface = FALSE # Actual colored surface
    
  ) %>% 
    plotly::layout(
      title = paste0('FBI table ', table, ': ', output_suffix),
      
      # Transparent hover: https://stackoverflow.com/a/47085052/1873521
      # hovermode = 'x unified', 
      hoverlabel = list(bgcolor = hover_background,
                        font = list(color = 'black')),
      
      scene = list(
        aspectmode = 'manual',
        
        # https://plotly.com/r/reference/layout/xaxis/#layout-xaxis
        # YEARS
        xaxis = list(title = "", # Years
                     nticks = n_ticks_x, 
                     autorange = "reversed",
                     ticks = "outside", tickwidth = 2, tickcolor = 'crimson', ticklen = 10,
                     tickfont = list(color = tick_labels_color, size = 12),
                     showspikes = show_spikes,
                     spikecolor = spike_color), #date
        # BIAS
        yaxis = list(title = "", # Bias labels
                     nticks = n_ticks_y, 
                     dtick = 1, # Show 1 every 2 ticks
                     gridwidth = 1, # Width of grid
                     autorange = TRUE,
                     automargin = FALSE,
                     showspikes = show_spikes,
                     spikecolor = spike_color,
                     # tickangle = -60,
                     # showgrid = FALSE,
                     # showline = TRUE,
                     tickfont = list(color = tick_labels_color, size = 12) # formats just font #family='Rockwell'
                     ),
        
        # INCIDENTS
        zaxis = list(title = "incidents",
                     showspikes = show_spikes,
                     spikecolor = spike_color,
                     separatethousands = TRUE,
                     ticks = "outside", tickwidth = 2, tickcolor = 'crimson', ticklen = 10, 
                     tickfont = list(color = tick_labels_color, size = 12),
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
    ) %>% 
    
    # 3D MESH on top of surface
    # https://plotly.com/r/reference/#surface-hidesurface
    # show: show 3d mesh
    # highlight: ADD highlight lines over surface
    add_surface(name = " ",
      contours = list(
        x = list(show = TRUE,
                 width = 1,
                 highlight = TRUE, 
                 highlightcolor = highlight_color),
        y = list(show = TRUE,
                 width = 1,
                 highlight = TRUE,
                 highlightcolor = highlight_color,
                 highlightwidth = 2),
        z = list(show = FALSE,
                 width = 1,
                 highlight = FALSE,
                 highlightcolor = highlight_color)
      )
    ) 
  
  
  PLOT
  
  
  # Export images --------------------------------------
  
  
  # Save png
    # Need to install kaleido (python): https://search.r-project.org/CRAN/refmans/plotly/html/save_image.html
  plotly::save_image(PLOT, paste0("outputs/PLOTs/surface/plot_FBI_t", table, "_surface_", output_suffix, ".png"), width = 1600, height = 900)
  
  # Save interactive html
  htmlwidgets::saveWidget(PLOT, paste0("outputs/PLOTs/surface/plot_FBI_t", table, "_surface_", output_suffix, ".html"), selfcontained = TRUE, libdir = "lib")
  
  return(PLOT)
  
}
