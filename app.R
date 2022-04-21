

# Initial data ------------------------------------------------------------

  suppressPackageStartupMessages(library(dplyr))
  suppressPackageStartupMessages(library(plotly))
  library(purrr)
  library(readr)
  library(shiny)
  library(shinythemes)
  library(tidyr)
  
  lapply(list.files(here::here("R"), full.names = TRUE, pattern = ".R$"), source)
  
  DF = read_csv2(here::here("outputs/DFs/FBI_table1_DF_long_clean.csv"))
  
  biases = DF %>% 
    count(bias_supra, bias_motivation) %>% 
    count(bias_supra) %>% filter(n > 1) %>% 
    pull(bias_supra)



# Shiny app ---------------------------------------------------------------
  
ui <- fluidPage(theme = shinytheme("flatly"),

    # titlePanel(""),

    sidebarLayout(
      
      sidebarPanel(width = 12, 
                   fluidRow(
                     
                   # shiny::column(width = 2,
                     # Get screen dimensions using js
                     tags$head(tags$script('
                                    var dimension = [0, 0];
                                    $(document).on("shiny:connected", function(e) {
                                        dimension[0] = window.innerWidth;
                                        dimension[1] = window.innerHeight;
                                        Shiny.onInputChange("dimension", dimension);
                                    });
                                    $(window).resize(function(e) {
                                        dimension[0] = window.innerWidth;
                                        dimension[1] = window.innerHeight;
                                        Shiny.onInputChange("dimension", dimension);
                                    });')
                               ),
                   # ),
                   shiny::column(width = 2,
                     selectInput(inputId = "bias_selected",
                                 label = "Bias supra:", 
                                 choices = c("*", biases), 
                                 selected = "*", 
                                 selectize = FALSE,
                                 size = 1), #length(biases) + 2),
                   ),
                   
                   shiny::column(width = 2,
                     selectInput(inputId = "arrange_by",
                                 label = "Arrange by:", 
                                 choices = c("bias_motivation", "bias_supra", "mean_value"), 
                                 selected = "mean_value"),
                     
                   ),
                   shiny::column(width = 2,
                     selectInput(inputId = "colorscale",
                                 label = "Colorscale:", 
                                 choices = c("Viridis", "Blackbody","Bluered","Blues","Cividis","Earth","Electric","Greens","Greys","Hot","Jet","Picnic","Portland","Rainbow","RdBu","Reds","YlGnBu","YlOrRd"), 
                                 selected = "Viridis"),
                   ),
                   
                   shiny::column(width = 1, offset = 1,
                   # TESTING
                     # hr(),
                     span(h6("(!): 'Relative' only works with Race"), style = "color:darkred"),
                   ),
                   
                   shiny::column(width = 2,
                     selectInput(inputId = "absolute_relative",
                                 label = "Absolute or relative data:", 
                                 choices = c("absolute", "relative"), 
                                 selected = "absolute", 
                                 selectize = FALSE),
                     
                   ),
                   
                   shiny::column(width = 2,
                     # DATA SOURCE
                     # hr(),
                     div(HTML("<B>Data:</B> <a href=\"https://www.fbi.gov/services/cjis/ucr/publications#Hate-Crime%20Statistics/\", target = \"_blank\">'Hate crime statistics, FBI'</a>"),
                         align = "left"),
                   ),
                   ),
      ),
                   
      
      mainPanel(width = 11,
                
                # verbatimTextOutput("dimension_display"),
                
                plotlyOutput("distPlot", width = "auto", height = "auto")
                # plotlyOutput("distPlot", width = "auto", height = "1000px")
                )
      )
    )

  
# Define server logic required to draw a histogram
server <- function(input, output) {

  # output$dimension_display <- renderText({paste(input$dimension[1], input$dimension[2])})
  
  # Observes input$dimension and uses it as input arguments in plot_FBI_t1_surface()
    observeEvent(input$dimension, {
      
      output$distPlot <- renderPlotly({
  
        # Data preparation
        DF_surface = prepare_data_FBI_t1_surface(DF = DF, supra_sub = "sub", filter_bias_supra = input$bias_selected, absolute_relative = input$absolute_relative, arrange_by = input$arrange_by)
        
        plot_title = gsub(":", "", input$bias_selected)
        if (plot_title == "*") plot_title = "ALL"
  
        # Plot
        plot_FBI_t1_surface(
          DF = DF_surface,
          output_suffix = plot_title,
          table = 1,
          save_outputs = FALSE,
          width = (0.95 * as.numeric(input$dimension[1])),
          height = (0.95 * as.numeric(input$dimension[2])),
          colorscale = input$colorscale,
          absolute_relative = input$absolute_relative
        )
  
      })
    })
}

shinyApp(ui = ui, server = server)
