run_cluster_analysis_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    fluidRow(
      tags$br(),
      
      column(
        width = 3,
        
        div(
          class = "boxInT",
          
          dropMenu(
            tag = prettyRadioButtons(inputId = ns("algorithm_choice"),
                                     label = tags$h5("Select an algorithm"),
                                     choices = c("K-partition clustering (kmeans)", 
                                                 "Hierarchical clustering"),
                                     selected = character(0),
                                     shape = "square",
                                     animation = "pulse",
                                     bigger = TRUE),
            
            selectInput(inputId = ns("more_algorithm_choice"),
                        label = tags$h5("More cluester algorithms"),
                        choices = NULL,
                        selected = NULL)
          ),
          
          shinyjs::hidden(
            prettyToggle(inputId = ns("km_reproducible"),
                         label_on = "Reproducible",
                         label_off = "Random",
                         value = TRUE,
                         thick = TRUE,
                         shape = "square",
                         animation = "pulse")
          )
        )
      ),
      column(
        width = 9,

        panel(
          class = "panel-color",
          
          tags$h4("Algorithm Summary"),
          
          uiOutput(outputId = ns("algorithm_summary"))
        )
      )
    ),
    
    tags$br(),
    
    fluidRow(
      column(
        width = 3,
        
        div(
          class = c("boxInT", "inputpad"),
          
          shinyjs::hidden(
            tags$h5(id = ns("su_header"), "Check data suitability"),
            tags$p(id = ns("su_exp"), paste("This is an optional check, Running this",
                                            "can take a lot of time for larger datasets.")),
            actionBttn(inputId = ns("run_data_suitability"),
                       label = "Yes",
                       style = "stretch",
                       color = "warning")
          ),
        
          tags$br(),
          tags$br(),
          
          prettyRadioButtons(inputId = ns("user_n_cluster_type"),
                             label = tags$h5("Number of cluster"),
                             choiceNames = c("Get optimal clusters",
                                             "Assign manually"),
                             choiceValues = c("get_optimal", "assign_manually"),
                             selected = character(0),
                             shape = "square",
                             animation = "pulse",
                             bigger = TRUE),
          
          chooseSliderSkin("Square"),
          shinyjs::hidden(
            sliderInput(inputId = ns("sli_number_centers"),
                        label = "",
                        min = 0, max = 10, value = 0, step = 1)
          ),
          
          tags$br(),
          tags$br(),
          
          actionBttn(inputId = ns("run_cluster_computation"),
                     label = "Run",
                     style = "material-flat",
                     color = "success"),
          
          tags$br(),
          tags$br(),
          
          actionBttn(inputId = ns("go_to_cluster_summary"),
                     label = "Cluster Summary",
                     style = "bordered",
                     color = "success")  |>
            shinyjs::hidden()
        )
      ),
      
      column(
        width = 9,
      
        panel(
          class = "panel-color",
          verbatimTextOutput(outputId = ns("print_data_sutability")) |>
            shinycssloaders::withSpinner(type = 4, 
                                         color = spinner_color,
                                         color.background = "white"),
          
          verbatimTextOutput(outputId = ns("print_optimal_centers")) |>
            shinycssloaders::withSpinner(type = 4, 
                                         color = spinner_color,
                                         color.background = "white")
        ),
        
        tags$br(),
        
        panel(
          class = "panel-color",
          reactableOutput(outputId = ns("cluster_table")) |>
            shinycssloaders::withSpinner(type = 4, 
                                         color = spinner_color,
                                         color.background = "white")
        ),
        
        tags$br(),
        
        div(
          class = "center-div",
          
          downloadButton(outputId = ns("download_cluster"),
                         label = "Download Data .csv")|>
            shinyjs::hidden()
        )
      )
    )
  )
}