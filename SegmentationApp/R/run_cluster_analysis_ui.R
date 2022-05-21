run_cluster_analysis_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    fluidRow(
      tags$br(),
      
      column(
        width = 4,
        
        div(
          class = c("boxInT", "inputpad"),
          
          dropMenu(
            tag = prettyRadioButtons(inputId = ns("algorithm_choice"),
                                     label = tags$h4("Select An Algorithm"),
                                     choices = c("K-partition clustering (kmeans)", 
                                                 "Hierarchical clustering"),
                                     selected = character(0),
                                     shape = "square",
                                     animation = "pulse",
                                     bigger = TRUE),
            
            selectInput(inputId = ns("more_algorithm_choice"),
                        label = tags$h5("More Cluester Algorithms"),
                        choices = NULL,
                        selected = NULL)
          )
        )
      ),
      
      column(
        width = 8,
        class = "boxOut",
        
        tags$h4("Algorithm Summary"),
        
        uiOutput(outputId = ns("algorithm_summary"))
      )
    ),
    
    tags$br(),
    
    fluidRow(
      column(
        width = 4,
        
        div(
          class = c("boxInT", "inputpad"),
          
          tags$br(),
          
          shinyjs::hidden(
            tags$h4(id = ns("su_header"), "Check Data Suitability"),
            tags$p(id = ns("su_exp"), paste("This is an optional check, Running this",
                                            "can take a lot of time for larger datasets.")),
            actionBttn(inputId = ns("run_data_suitability"),
                       label = "Yes",
                       style = "minimal",
                       color = "warning")
          ),
        
          tags$br(),
          tags$br(),
          
          tags$h4("Get Optimal Number of Clusters"),
          actionBttn(inputId = ns("get_optimal_center"),
                     label = "Yes",
                     style = "minimal",
                     color = "primary"),
          
          tags$br(),
          tags$br(),
          
          tags$h4(id = ns("nc_header"), "Assign Number of Clusters"),
          tags$p(id = ns("nc_exp"), 
                 paste("This dose not require getting",
                       "the optimal number of clusters anymore.")),
          chooseSliderSkin("Square"),
          sliderInput(inputId = ns("sli_number_centers"), 
                      label = "",
                      min = 0, max = 10, value = 0, step = 1),
          
          tags$br(),
          tags$br(),
          tags$br(),
          
          actionBttn(inputId = ns("run_cluster_computation"),
                     label = "Run",
                     style = "material-flat",
                     color = "success"),
          
          tags$br(),
          tags$br(),
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
        width = 8,
        
        div(
          class = "boxOut",
          
          verbatimTextOutput(outputId = ns("print_data_sutability")) |>
            shinycssloaders::withSpinner(type = 4, color.background = "white"),
          
          verbatimTextOutput(outputId = ns("print_optimal_centers")) |>
            shinycssloaders::withSpinner(type = 4, color.background = "white")
        ),
        
        tags$br(),
        tags$br(),
        
        div(
          class = "boxOut",
          
          reactableOutput(outputId = ns("cluster_table")) |>
            shinycssloaders::withSpinner(type = 4, color.background = "white")
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