assign_segment_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    fluidRow(
      column(
        width = 3,
        align = "center",
        
        div(
          class = "boxInT",
          
          h3("Method Of Assignment"),
          dropMenu(
            tag =  actionBttn(inputId = ns("manual_assginment_method"), 
                              label = "Manual Assignment",
                              style = "stretch",
                              color = "primary",
                              size = "md"),
            
            
            uiOutput(outputId = ns("segment_values")),
            
            tags$br(),
            tags$br(),
            
            prettyCheckbox(inputId = ns("manual_drop_cluster"),
                           label  = "Drop Cluster Variable",
                           shape  = "square",
                           status = "danger",
                           bigger = TRUE),
            
            tags$br(),
            tags$br(),
            
            actionButton(inputId = ns("assign_manual"),
                         label = "Assign",
                         class = "monitor")
          ),
          
          tags$br(),
          tags$br(),
          
          dropMenu(
            tag = actionBttn(inputId = ns("aggregate_assignment_method"), 
                             label = "Aggregate Assignment",
                             style = "stretch",
                             color = "primary",
                             size  = "md"),
            
            actionBttn(inputId = ns("show_agg_info"), 
                       label = "More Info", 
                       style = "material-circle",
                       icon = icon("info-circle")),
            uiOutput(outputId = ns("agg_info")),
            
            tags$br(),
            
            pickerInput(inputId = ns("agg_numeric_variable"),
                        label = h4("Select a variable"),
                        choices = NULL),
            
            tags$br(),
            h4("Segments"),
            textInput(inputId = ns("agg_segments_value"),
                      label = "values must be separated with comma",
                      value = "",
                      placeholder = "Segments...."),
            
            tags$br(),
            
            pickerInput(inputId = ns("agg_function"),
                        label = h4("Use Function"),
                        choices = c("Average" = "mean",
                                    "Median"  = "median",
                                    "Sum"     = "sum",
                                    "Minimum" = "min",
                                    "Maximum" = "max"),
                        selected = "mean"),
            
            tags$br(),
            
            prettyCheckbox(inputId = ns("agg_drop_cluster"),
                           label  = "Drop Cluster Variable",
                           shape  = "square",
                           status = "danger",
                           bigger = TRUE),
            
            tags$br(),
            
            actionButton(inputId = ns("assign_agg"),
                         label = "Assign",
                         class = "monitor")
          ),
          
          tags$br(),
          tags$br(),
          
          actionBttn(inputId = ns("go_to_segment_summary"),
                     label = "Segmentation Summary",
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
          
          reactableOutput(outputId = ns("segment_table")) |>
            shinycssloaders::withSpinner(type = 4,
                                         color = spinner_color,
                                         color.background = "white")
        ),
        
        tags$br(),
        
        div(
          class = "center-div",
          
          downloadButton(outputId = ns("download_segment"),
                         label = "Download Data .csv") |>
            shinyjs::hidden()
        )
      )
    )
  )
}


