segment_summary_ui <- function(id) {
  ns <- NS(id)
  
  shiny::tagList(
    fluidRow(
      column(
        width = 3,
        
        div(
          class = c("boxIn", "inputpad"),
          
          pickerInput(inputId = ns("frist_variable"),
                      label   = tags$h4("Select a Variable"),
                      choices = NULL),
          
          tags$br(),
          
          
          pickerInput(inputId = ns("second_variable"),
                      label   = tags$h4("Select a Variable (Optional)"),
                      choices = NULL),
          
          tags$br(),
          
          shinyjs::hidden(
            pickerInput(inputId = ns("aggregate_dash_fun"),
                        label   = tags$h5("Aggregate Function"),
                        choices = c("Average" = "mean",
                                    "Median"  = "median",
                                    "Sum"     = "sum",
                                    "Minimum" = "min",
                                    "Maximum" = "max")),
            
            tags$br(),
            
            numericInput(inputId = ns("n_category"),
                         label   = tags$h5("Number of Observations to plot"),
                         min = 5, max = 15, value = 10, step = 1)
          ),
          
          tags$br(),
          
          prettyCheckbox(inputId = ns("add_table"),
                         label  = "Add a Table",
                         value  = FALSE,
                         status = "primary",
                         shape  = "square",
                         animation = "pulse",
                         bigger = TRUE),
          
          tags$br(),
          
          numericInput(inputId = ns("n_dash_rows"),
                       label = "Number of Rows to Display",
                       min = 5, max = 100, value = 10, step = 1),
          
          tags$br(),
          tags$br(),
          
          actionBttn(inputId = ns("create_display"),
                     label = "Display",
                     style = "bordered",
                     color = "success",
                     size  = "lg")
        )
      ),
      
      column(
        width = 9,
        
        div(
          class = "boxOut",
        
          plotOutput(outputId = ns("plot_output")) |>
            shinycssloaders::withSpinner(type = 4),
          
          tags$br(),
          tags$br(),
          
          plotOutput(outputId = ns("t_plot_output")),
          reactableOutput(outputId = ns("p_table_output")),
          
          tags$br(),
          tags$br(),
          
          reactableOutput(outputId = ns("table_output")) |>
            shinycssloaders::withSpinner(type = 4)    
        )
      )
    )
  )
}