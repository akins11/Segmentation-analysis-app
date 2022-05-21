cluster_summary_ui <- function(id) {
  ns <- NS(id)

  shiny::tagList(
    fluidRow(
      align = "center",
      
      column(
        width = 2,
        
        div(
          class = c("boxIn", "inputpad"),
          
          tags$p(
            "
            Each cluster can be summarised using an aggregate function with
            selected numerical and character variables, This can show the 
            relationship between selected variables and each cluster.
            "
          )
        )
        
      ),
      
      column(
        width = 6,
        div(
          class = "boxOut",
          
          plotOutput(outputId = ns("cluster_count_plot")) |>
            shinycssloaders::withSpinner(type = 4)
        )
      ),
      
      column(
        width = 4,
        div(
          class = "boxOut",
          
          reactableOutput(outputId = ns("cluster_count_table")) |>
            shinycssloaders::withSpinner(type = 4)
        )
      )
    ),
    
    # tags$br(),
    tags$br(),
    
    uiOutput(outputId = ns("is_with_charcter_var")),
    
    # tags$br(),
    tags$br(),
    
    fluidRow(
      align = "center",
      
      column(
        width = 2,
        
        div(
          class = c("boxIn", "inputpad"),
          
          pickerInput(inputId = ns("num_vars_stat_sumy"),
                      label   = "Select Variable(s)",
                      choices = NULL,
                      multiple = TRUE),
          
          tags$br(),
          
          pickerInput(inputId = ns("stat_vars"),
                      label   = "Aggregate Function",
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
                     style = "minimal",
                     color = "success")
        )
        
      ),
      
      column(
        width = 6,
        
        div(
          class = "boxOut",
          
          plotOutput(outputId = ns("stat_summary_plot")) |>
            shinycssloaders::withSpinner(type = 4)
        )
      ),
      
      column(
        width = 4,
        
        div(
          class = "boxOut",
          
          reactableOutput(outputId = ns("stat_summary_table")) |>
            shinycssloaders::withSpinner(type = 4)
        )
      )
    ),
  
    # tags$br(),
    tags$br(),
    
    fluidRow(
      align = "center",
      
      column(
        width = 2,
        
        div(
          class =  c("boxIn", "inputpad"),
          
          pickerInput(inputId = ns("num_var_x"),
                      label = "X Variable",
                      choices = NULL),
          
          tags$br(),
          
          pickerInput(inputId = ns("num_var_y"),
                      label = "Y Variable",
                      choices = NULL),
          
          tags$br(),
          
          actionBttn(inputId = ns("run_rel_plot"),
                     label = "Run",
                     style = "minimal",
                     color = "success"),
          
          tags$br(),
          tags$br(),
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
        
        div(
          class = "boxOut",
          
          plotOutput(outputId = ns("cluster_scatter_plot")) |>
            shinycssloaders::withSpinner(type = 4)
        )
      )
    )
  )
}