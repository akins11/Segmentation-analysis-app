data_choice_ui <- function(id) {
  ns <- NS(id)
  
  shiny::tagList(
    fluidRow(
      align = "center",
      tags$br(),
      
      column(
        width = 2,

        div(
          class = c("boxIn", "inputMag"),
          
          dropMenu(tag = actionBttn(inputId = ns("btn_upload_data"),
                                    label = "Upload Data",
                                    style = "stretch",
                                    color = "primary"),
                   
                   fileInput(inputId = ns("fi_upload_data"),
                             label  = "",
                             accept = c(".csv", ".tsv"))
          ),
          
          tags$br(),
          
          actionBttn(inputId = ns("btn_demo_data"),
                     label = "Use Demo Data",
                     style = "stretch",
                     color = "primary"),
          
          tags$br(),
          tags$br(),
          
          uiOutput(outputId = ns("drop_missing_value")),
          
          tags$br(),
          
          numericInput(inputId = ns("ni_n_rows"),
                       label = tags$h6("Number Of rows"),
                       min   = 3, max = 100, step = 1, value = 10, 
                       width = "180px"),
          
          tags$br(),
          tags$br(),
          
          shinyjs::hidden(
            actionBttn(inputId = ns("btn_select_vars"),
                       label = "Select Variables",
                       style = "bordered",
                       color = "success")
          )
        )
      ),
      
      column(
        width = 10,
        
        panel(
          class = "panel-color",
          h4("Data Preview"),
          tags$br(),
          
          reactableOutput(outputId = ns("rt_upload_data")) |>
            shinycssloaders::withSpinner(type = 4,
                                         color = spinner_color,
                                         color.background = "white")
        ),
        
        panel(
          class = c("missing-info", "panel-color"),
          uiOutput(outputId = ns("data_info")),
          
          tags$br(),
          
          uiOutput(outputId = ns("drop_missing_value_info"))
        )
      )
    )
  )
}