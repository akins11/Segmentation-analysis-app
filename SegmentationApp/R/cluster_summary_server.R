cluster_summary_server <- function(id, clust_data, parent_session) {
  stopifnot(is.reactive(clust_data))
  
  moduleServer(
    id = id,
    
    module = function(input, output, session) {
      # Cluster count ---------------------------------------------------------|
      cluster_count <- reactive({
        req(clust_data())
        
        list(
          table = count_clusters(df = clust_data(), output_type = "table"),
          plot  = count_clusters(df = clust_data(), output_type = "plot")
        )
      }) |> 
        bindEvent(clust_data(), label = "cluster_count_reactive")
      
      
      output$cluster_count_table <- renderReactable({
        clean_reactable_names(x = cluster_count()$table, relocate = FALSE) |>
          summary_reactable(include = "cluster")
      })
      output$cluster_count_plot <- renderPlot({ cluster_count()$plot })
      
      
      
      # Character count -------------------------------------------------------|
      is_char_avaliable <- reactive({
        is_with_type(df = clust_data(), 
                     with_type = "character")
      })
      chr_names <- reactive({
        req(clust_data())
        
        if (isTRUE(is_char_avaliable())) {
          get_var_type_names(df   = clust_data(),
                             type = "character",
                             remove_var = c(".cluster", "row_id"))
        } else {
          character(0)
        }
      }) |>
        bindEvent(clust_data(), label = "chr_var_names_reactive")
      
      output$is_with_charcter_var <- renderUI({
        req(is_char_avaliable(), chr_names())
        
        if (isTRUE(is_char_avaliable())) {
          
          fluidRow(
            align = "center",
            
            column(
              width = 2,
              
              div(
                class = c("boxIn", "inputpad"),
                
                pickerInput(inputId = session$ns("char_var_count"),
                            label   = "Select A Variable",
                            choices = chr_names(),
                            options = pickerOptions(title = "Nothing Selected")),
                
                tags$br(),
                
                numericInput(inputId = session$ns("chr_count_nrows"),
                             label   = "Number Of Rows",
                             min = 3, max = 100, value = 10)
              )
            ),
            
            column(
              width = 6,
              
              div(
                class = "boxOut",
                
                plotOutput(outputId = session$ns("chr_count_plot")) |>
                  shinycssloaders::withSpinner(type = 4)
              )
            ),
            
            column(
              width = 4,
              
              div(
                class = "boxOut",
                
                reactableOutput(outputId = session$ns("chr_count_table"))
              )
            )
          )
        }
      })
      
      char_count_summary <- reactive({
        req(clust_data(), input$char_var_count)
        
        list(
          table = chr_count_cluster(df = clust_data(), 
                                    chr_var = input$char_var_count,
                                    output_type = "table"),
          
          plot = chr_count_cluster(df = clust_data(),
                                   chr_var = input$char_var_count,
                                   output_type = "plot")
        )
      }) |>
        bindEvent(input$char_var_count, label = "chr_count_sumy_reactive")
      
      
      output$chr_count_table <- renderReactable({
        clean_reactable_names(x = char_count_summary()$table, 
                              include = "cluster") |>
          summary_reactable(include = "cluster",
                            page_size = input$chr_count_nrows)
      })
      output$chr_count_plot <- renderPlot({ char_count_summary()$plot })
      
    
      
      # Cluster stat summary --------------------------------------------------|
      observe({
        req(clust_data())
        
        num_names <- get_var_type_names(df   = clust_data(),
                                        type = "numeric")
        
        updatePickerInput(session = session,
                          inputId = "num_vars_stat_sumy",
                          choices = num_names,
                          options = pickerOptions(title = "Nothing Selected"))
      }) |>
        bindEvent(clust_data(), label = "num_vars_stat_sumy_observe")
      
      
      cluster_stat_sumy <- reactive({
        req(clust_data(), input$num_vars_stat_sumy, input$stat_vars)
        
        list(
          table = cluster_stat_summary(df = clust_data(),
                                       num_variables = input$num_vars_stat_sumy,
                                       output_type   = "table"),
          
          plot = cluster_stat_summary(df = clust_data(),
                                      num_variables = input$num_vars_stat_sumy,
                                      plot_stat_var = input$stat_vars,
                                      output_type   = "plot")
        )
      }) |>
        bindEvent(input$run_stat_sumy, label = "cluster_stat_sumy_reactive")
      
      output$stat_summary_plot <- renderPlot({cluster_stat_sumy()$plot })
      
      output$stat_summary_table <- renderReactable({
        clean_reactable_names(x = cluster_stat_sumy()$table, relocate = FALSE) |>
          summary_reactable(include = "cluster")
      })
      
      # Relationship plot -----------------------------------------------------|
      observe({
        req(clust_data())
        
        num_names <- get_var_type_names(df   = clust_data(),
                                        type = "numeric")
        
        updatePickerInput(session = session,
                          inputId = "num_var_x",
                          choices = num_names,
                          options = pickerOptions(title = "Nothing Selected"))
        
        updatePickerInput(session = session,
                          inputId = "num_var_y",
                          choices = num_names,
                          options = pickerOptions(title = "Nothing Selected"))
        
      }) |>
        bindEvent(clust_data(), label = "rel_plot_observer")
      
      relationship_plot <- reactive({
        req(clust_data(), input$num_var_x, input$num_var_y)
        
        cluster_relationship_plot(df = clust_data(),
                                  num_varx = input$num_var_x,
                                  num_vary = input$num_var_y)
        
      }) |>
        bindEvent(input$run_rel_plot, label = "rel_plot_reactive")
      
      output$cluster_scatter_plot <- renderPlot({ relationship_plot() })
      
      
      
      # Switch to assign segment tab ------------------------------------------|
      observe({
        updateNavbarPage(session  = parent_session,
                         inputId  = "navbar_container",
                         selected = "tab_assign_segment")
      }) |>
        bindEvent(input$go_to_assign_segment,
                  label = "go_to_cluster_summary_observe")
    }
  )
}