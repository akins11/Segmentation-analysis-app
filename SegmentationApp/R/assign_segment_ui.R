assign_segment_ui <- function(id) {
  ns <- NS(id)
  shiny::tagList(
    fluidRow(
      column(
        width = 4,
        align = "center",
        
        div(
          class = c("boxInT", "inputpad"),
          
          h3("Method Of Assignment"),
          dropMenu(
            tag =  actionBttn(inputId = ns("manual_assginment_method"), 
                              label = "Manual Assignment",
                              style = "minimal",
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
                             style = "minimal",
                             color = "primary",
                             size  = "md"),
            
            
            tags$p(
              "
              Select a numerical variable which will be summarise using the choosen
              aggregate function, The calculation will be done for each cluster and
              the result will be sorted from the largest to the smallest.
              "
            ),
            tags$strong("Make sure the segments provided are sorted from the 
                        best to the least."),
            
            tags$br(),
            tags$br(),
            
            pickerInput(inputId = ns("agg_numeric_variable"),
                        label = h4("Select A variable"),
                        choices = NULL),
            
            tags$br(),
            h4("Segments"),
            textInput(inputId = ns("agg_segments_value"),
                      label = "values must be separated with comma or space",
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
            tags$br(),
            
            actionButton(inputId = ns("assign_agg"),
                         label = "Assign",
                         class = "monitor")
          ),
          
          tags$br(),
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
        width = 8,
      
        tags$br(),
        
        div(
          class = "boxOut",
          
          reactableOutput(outputId = ns("segment_table")) |>
            shinycssloaders::withSpinner(type = 4)
        ),
        
        tags$br(),
        
        div(
          class = "center-div",
          
          downloadButton(outputId = ns("download_segment"),
                         label = "Download Data .csv")|>
            shinyjs::hidden()
        )
      )
    )
  )
}