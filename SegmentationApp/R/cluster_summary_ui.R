cluster_summary_ui <- function(id) {
  ns <- NS(id)

  shiny::tagList(
    fluidRow(
      align = "center",
      
      column(
        width = 2,
        
        panel(
          class = "panel-color",
          
          statiCard(
            value = 0, 
            subtitle = "Clusters", 
            icon = icon("chart-pie"),
            background = cs_col_BC,
            color = "white",
            id = ns("cluster_card")
          )
        ),
        
      ),
      
      column(
        width = 6,

        panel(
          class = "panel-color",
          
          plotOutput(outputId = ns("cluster_count_plot")) |>
            shinycssloaders::withSpinner(type = 4, 
                                         color = spinner_color,
                                         color.background = "white")
        )
      ),
      
      column(
        width = 4,

        panel(
          class = "panel-color",
          
          reactableOutput(outputId = ns("cluster_count_table")) |>
            shinycssloaders::withSpinner(type = 4,
                                         color = spinner_color,
                                         color.background = "white")
        )
      )
    ),
    
    tags$br(),
    
    uiOutput(outputId = ns("is_with_charcter_var")),
    
    tags$br(),
    
    fluidRow(
      align = "center",
      
      column(
        width = 2,
        
        div(
          class = "boxIn",
          
          pickerInput(inputId = ns("num_vars_stat_sumy"),
                      label   = tags$h5("Select variable(s)"),
                      choices = NULL,
                      multiple = TRUE),
          
          tags$br(),
          
          pickerInput(inputId = ns("stat_vars"),
                      label   = tags$h5("Aggregate function"),
                      choices = c("Average" = "mean",
                                  "Median"  = "median",
                                  "Sum"     = "sum",
                                  "Minimum" = "minimum",
                                  ".25 Quantile" = "quantile_25",
                                  ".75 Quantile" = "quantile_75",
                                  "Maximum" = "maximum"),
                      selected = "mean"),
          
          tags$br(),
          
          actionBttn(inputId = ns("run_stat_sumy"),
                     label = "Run",
                     style = "stretch",
                     color = "success")
        )
        
      ),
      
      column(
        width = 6,
        
        panel(
          class = "panel-color",
          
          plotOutput(outputId = ns("stat_summary_plot")) |>
            shinycssloaders::withSpinner(type = 4,
                                         color = spinner_color,
                                         color.background = "white")
        )
      ),
      
      column(
        width = 4,
  
        panel(
          class = "panel-color",
          
          reactableOutput(outputId = ns("stat_summary_table")) |>
            shinycssloaders::withSpinner(type = 4,
                                         color = spinner_color,
                                         color.background = "white")
        )
      )
    ),
  
    tags$br(),
    
    fluidRow(
      align = "center",
      
      column(
        width = 2,
        
        div(
          class =  "boxIn",
          
          pickerInput(inputId = ns("num_var_x"),
                      label = tags$h5("X variable"),
                      choices = NULL),
          
          tags$br(),
          
          pickerInput(inputId = ns("num_var_y"),
                      label = tags$h5("Y variable"),
                      choices = NULL),
          
          tags$br(),
          
          actionBttn(inputId = ns("run_rel_plot"),
                     label = "Run",
                     style = "stretch",
                     color = "success"),
          
          tags$br(),
          tags$br(),
          
          actionBttn(inputId = ns("go_to_assign_segment"),
                     label = "Assign Segment",
                     style = "bordered",
                     color = "success")
        )
      ),
      
      column(
        width = 10,

        panel(
          class = "panel-color",
          
          plotOutput(outputId = ns("cluster_scatter_plot")) |>
            shinycssloaders::withSpinner(type = 4,
                                         color = spinner_color,
                                         color.background = "white")
        )
      )
    )
  )
}