data_choice_ui <- function(id) {
  ns <- NS(id)
  
  shiny::tagList(
    fluidRow(
      align = "center",
      tags$br(),
      
      column(
        width = 3,

        div(
          class = c("boxIn", "inputMag"),
          
          dropMenu(tag = actionBttn(inputId = ns("btn_upload_data"),
                                    label = "Upload Data",
                                    style = "minimal",
                                    color = "primary"),
                   
                   fileInput(inputId = ns("fi_upload_data"),
                             label  = "Upload A Data",
                             accept = c(".csv", ".tsv"))
          ),
          
          tags$br(),
          tags$br(),
          
          actionBttn(inputId = ns("btn_demo_data"),
                     label = "Use Demo Data",
                     style = "minimal",
                     color = "primary"),
          
          tags$br(),
          tags$br(),
          tags$br(),
          
          numericInput(inputId = ns("ni_n_rows"),
                       label = "Number Of Rows To Display",
                       min   = 3, max = 100, step = 1, value = 10),
          
          tags$br(),
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
        width = 9,
        class = "boxOut",
        
        h4("Data Preview"),
        tags$br(),
        
        reactableOutput(outputId = ns("rt_upload_data")) |>
          shinycssloaders::withSpinner(type = 4,
                                       color.background = "white")
      )
    )
  )
}