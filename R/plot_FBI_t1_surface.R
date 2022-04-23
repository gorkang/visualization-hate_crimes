plot_FBI_t1_surface <- function(DF, output_suffix, table, save_outputs = TRUE, height = NULL, width = NULL, colorscale = "Viridis", absolute_relative = "absolute") {
  
  # DEBUG
  # DF = DF_matrix
  
  # targets::tar_load(DF_FBI_t1_surface_sexual)
  # DF = DF_FBI_t1_surface_sexual
  # output_suffix = "ALL"
  # table = 1
  # colorscale = "Viridis"
  # height = NULL
  # width = NULL
  # absolute_relative = "absolute"

  # DF = DF_FBI_t1_surface_ALL
  # DF = DF_FBI_t1_surface_gender
  # output_suffix = "Race"
  # table = 1

    
  # https://plotly-r.com/index.html
  # https://plotly.com/r/reference/#surface
  
  
  # Manual parameters ----------------------------------------------------
  
    # Values for each axis
    x_axis = colnames(DF) # x_axis = as.Date(colnames(DF), format = "%Y")
    y_axis = rownames(DF) # GET RID OF BR in labels: # y_axis_labels = gsub(" <br> ", " ", rownames(DF))
    z_axis = DF # OR t(DF)
    
    # Visual characteristics
    show_spikes = FALSE # Straight lines. Not very useful if we have highlight
    
    # COLORS
    spike_color = "#111111"
    highlight_color = "#41a7b3"
    hover_background = "rgba(255,255,255,0.75)" # OR "#ffffff"
    tick_labels_color = "#222222"
  
    # We can define our own thresholds and colors  
      # color_thresholds = seq(0, 1, length.out = 10)
      # color_names = viridisLite::viridis(10, alpha = 1, begin = 0, end = 1, direction = 1, option = "D")# c("white", "grey", "black", "yellowgreen", "#7fff00")
    

  # Automatic parameters ----------------------------------------------------
  
    name_z = ifelse(absolute_relative == "absolute", "Incidents", "Incidents/100,000")
    
    # Number of ticks  
    n_ticks_x = length(x_axis)
    n_ticks_y = length(y_axis)
    
    # Aspect ratio of plot depends on nunmber of lables in y axis
    aspect_ratio_y = ifelse(length(y_axis) > 2, 2, 1)
    

  
  # plot_ly --------------------------------------------------------------------

  PLOT = plotly::plot_ly(height = height, width = width,
    x = ~ x_axis,
    y = ~ y_axis,
    z = ~ z_axis,
    type = "surface",
    connectgaps = FALSE, # FILLS NA with 0 # Freezes when overing over NA value
    
    showlegend = FALSE,
    showscale = FALSE, # legend dissapears
    hidesurface = FALSE, # Actual colored surface
    
    
    # colorbar = list(title = "Incidents", ticks = "outside"),
    
    # IF USING A COMMON COLOR PALLETE TO ALL PLOTS
    # REVIEW: If we want a common scale for all plots, we need to define the common reference (all data, inside each supra group,... ?)
      # cmin = 0, 
      # cmax = max(z_axis, na.rm = TRUE), # Define min and max values for the extreme colors
      # colorscale = list(color_thresholds, color_names), # color_thresholds, color_names defined above
    
    colorscale = colorscale, # Blackbody,Bluered,Blues,Cividis,Earth,Electric,Greens,Greys,Hot,Jet,Picnic,Portland,Rainbow,RdBu,Reds,Viridis,YlGnBu,YlOrRd.
    opacity = 1, # Transparency of surface 0-1
    
    # Template for hover
    hovertemplate = paste0(
      "<b>%{y}</b><br>",
      "Year: %{x:%Y}<br>",
      name_z, " : %{z}",
      "<extra></extra>"
    )
    # hoverlabel = list(bgcolor = "rgba(255,255,255,0.75)"),
    
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
        zaxis = list(title = name_z,
                     showspikes = show_spikes,
                     spikecolor = spike_color,
                     separatethousands = TRUE,
                     ticks = "outside", tickwidth = 2, tickcolor = 'crimson', ticklen = 10, 
                     tickfont = list(color = tick_labels_color, size = 12),
                     col = 1), 
        
        # Aspect ratio of plot
        aspectratio = list(x = 1, y = aspect_ratio_y, z = 1),
        
        # Initial position of camera
        # https://plotly.com/r/reference/#layout-scene-camera
        # x: years y: bias z: incidents
        camera = list(
          eye = list(x = 2, y = 1, z = 1),
          center = list(x = 0.5, y = 0.2, z = 0),
          up = list(x = 0, y = 0, z = 1)
          # eye = list(x = 0.5, y = 3, z = 1),
          # center = list(x = 0, y = 0, z = -0.5), # - up +down
          # up = list(x = 0, y = 0, z = 1) # which up 0/1
          )
      )
    ) %>% 
    
    # 3D MESH on top of surface
    # https://plotly.com/r/reference/#surface-hidesurface
    # show: show 3d mesh
    # highlight: ADD highlight lines over surface
    plotly::add_surface(name = " ",
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

  if (save_outputs == TRUE) {
  
    # Save png
      # Need to install kaleido (python): https://search.r-project.org/CRAN/refmans/plotly/html/save_image.html
    plotly::save_image(PLOT, paste0("outputs/PLOTs/surface/plot_FBI_t", table, "_surface_", output_suffix, ".png"), width = 1600, height = 900)
  
    # Save interactive html
    htmlwidgets::saveWidget(PLOT, paste0("outputs/PLOTs/surface/plot_FBI_t", table, "_surface_", output_suffix, ".html"), selfcontained = TRUE, libdir = "lib")
  }
  
  return(PLOT)

}
