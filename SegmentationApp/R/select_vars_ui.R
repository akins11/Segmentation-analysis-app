select_vars_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    fluidRow(
      align = "center",
      
      column(
        width = 4,
        tags$br(),
        
       div(
         class = c("boxIn", "inputMag"),
         
         h4("Select Variables For Cluster Analysis"),
         
         tags$p(
           "
           A minimum of two variable must be selected.
           Numeric variables are more preferred than character variables
           and also Fewer variables are best for computational performance.
           "
         ),
         pickerInput(inputId = ns("select_cluster_vars"),
                     label = "",
                     choices = NULL,
                     multiple = TRUE,
                     options = list(`actions-box` = TRUE,
                                    `select-all-text` = "Select All",
                                    `none-selected-text` = "No Selection Yet",
                                    `multiple-separator` = " | ")),
         
         tags$br(),
         
         pickerInput(inputId  = ns("select_data_id"),
                     label    = h4("Select an ID variable (if avaliable)"),
                     choices  = NULL,
                     selected = "No Selection",
                     multiple = FALSE),
         
         tags$br(),
         
         numericInput(inputId = ns("ni_n_sv_rows"),
                      label   = h4("Number of rows to display"),
                      min     = 3, max = 100, value = 5, step = 1),
         
         tags$br(),
         
         prettyToggle(inputId = ns("pt_stand_vars"),
                      label_on = "Standardize",
                      label_off = "Don't Standardize",
                      value = TRUE,
                      status_on = "success",
                      status_off = "primary",
                      bigger = TRUE),
         
         tags$br(),
         tags$br(),
         
         actionBttn(inputId = ns("btn_go_to_cluster_algo"),
                    label = "Run Cluster Analysis",
                    style = "bordered",
                    color = "success") |>
           shinyjs::hidden()
       )
      ),
      
      column(
        width = 8,
        
        tags$br(),
        
        div(
          class = "boxOut",
          
          tags$h4("Selected Variables"),
          
          reactableOutput(outputId = ns("selected_vars_out")) |>
            shinycssloaders::withSpinner(type = 4, color.background = "white")
        ),
        
        tags$br(),
        
        div(
          class = "boxOut",
          
          tags$h4("Restructured Data"),
          
          reactableOutput(outputId = ns("engineered_data")) |>
            shinycssloaders::withSpinner(type = 4, color.background = "white")
        )
      )
      
    )
  )
}