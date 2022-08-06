select_vars_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    fluidRow(
      align = "center",
      
      column(
        width = 3,
        tags$br(),
        
       div(
         class = c("boxIn", "inputpad"),
         
         h5(id = "vars-id",  "Select variables for cluster analysis"),
         
         tags$p(
           id = "select-vars-info",
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
                     label    = h5("Select an ID variable (if avaliable)"),
                     choices  = NULL,
                     selected = "No Selection",
                     multiple = FALSE),
         
         tags$br(),
         
         numericInput(inputId = ns("ni_n_sv_rows"),
                      label   = h6("Number of rows"),
                      min     = 3, max = 100, value = 5, step = 1,
                      width = "60%"),
         
         tags$br(),
         
         prettyToggle(inputId = ns("pt_stand_vars"),
                      label_on = "Standardize",
                      label_off = "Don't standardize",
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
        width = 9,
        
        tags$br(),
        
        panel(
          class = "panel-color",
          tags$h4("Selected Variables"),
          
          reactableOutput(outputId = ns("selected_vars_out")) |>
            shinycssloaders::withSpinner(type = 4, 
                                         color = spinner_color,
                                         color.background = "white")
        ),
        
        tags$br(),
        
        panel(
          class = "panel-color",
          tags$h4("Restructured Data"),
          
          reactableOutput(outputId = ns("engineered_data")) |>
            shinycssloaders::withSpinner(type = 4, 
                                         color = spinner_color,
                                         color.background = "white")
        )
      )
      
    )
  )
}