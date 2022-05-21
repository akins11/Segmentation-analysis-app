assign_segment_server <- function(id, clust_data, parent_session) {
  stopifnot(is.reactive(clust_data))
  
  moduleServer(
    id = id,
    
    module = function(input, output, session) {
      # Manual Assignment -----------------------------------------------------|
      num_clusters <- reactive({
        req(clust_data())
        get_num_clusters(df = clust_data())
      })
      
      output$segment_values <- renderUI({
        req(num_clusters())
        lapply(seq_len(num_clusters()), function(.x) {
          textInputIcon(inputId = session$ns(paste0("seg_", .x)),
                        label = "",
                        icon  = list(paste("cluster", .x)),
                        value = "")
        })
      })
      
      combine_segments <- reactive({
        req(clust_data())
        cs <- c()
        for (i in seq_len(num_clusters())) {
          cs <- c(cs, input[[paste0("seg_", i)]])
        }
        cs 
      })
      
      manual_seg_assigment <- reactive({
        req(clust_data(), combine_segments())
        
        if (input$assign_manual > 0) {
          assign_seg_manually(df = clust_data(),
                              segment = combine_segments(),
                              drop_cluster = input$manual_drop_cluster)
        }
        
      }) |>
        bindEvent(input$assign_manual, label = "manual_seg_reactive")
      
      
      
      # Aggregate Assignment --------------------------------------------------|
      observe({
        numeric_vars_names <- get_var_type_names(df = clust_data(), 
                                                 type = "numeric")
        
        updatePickerInput(session = session,
                          inputId = "agg_numeric_variable",
                          choices = numeric_vars_names,
                          options = pickerOptions(title = "Nothing Selected"))
      })
      
      user_supplied_segments <- reactive({
        req(input$agg_segments_value)
        get_user_segments(input$agg_segments_value)
      })
      
      observe({
        req(num_clusters(), user_supplied_segments())
        
        if (num_clusters() != length(user_supplied_segments())) {
          show_alert(title = "Wrong Number Of Segments",
                     text = paste("There are", num_clusters(), "number of",
                                  "clusters but you supplied",
                                  length(user_supplied_segments()),
                                  "segments."),
                     type = "error")
        }
      }) |>
        bindEvent(input$assign_agg)
      
      
      agg_seg_assignment <- reactive({
        req(clust_data(), user_supplied_segments(), input$agg_numeric_variable,
            input$agg_function)
        
        if (input$assign_agg > 0 ) {
          assign_seg_num_var(df = clust_data(),
                             num_var = input$agg_numeric_variable,
                             str_fun = input$agg_function,
                             segment = user_supplied_segments(),
                             drop_cluster = input$agg_drop_cluster)
        }
      }) |>
        bindEvent(input$assign_agg, label = "agg_seg_reactive")
      
      
      
      # segment data ----------------------------------------------------------|
      segment_dataframe <- reactive({
        # req(input$last_click)
        if (input$assign_manual > 0 && 
            input$last_click == "assign_segments-assign_manual") {
          manual_seg_assigment()

        } else if (input$assign_agg > 0 && 
                   input$last_click == "assign_segments-assign_agg") {
          agg_seg_assignment()
        }
      })
      
      output$segment_table <- renderReactable({
        req(segment_dataframe())

        clean_reactable_names(x = segment_dataframe(),
                              relocate = TRUE,
                              include = "segment") |>
          cluster_segment_reactable(include = "segment", page_size = 20)
      })

      # Download segment data -------------------------------------------------|
      observe({
        shinyjs::show(id = "download_segment")
      }) |>
        bindEvent(segment_dataframe(), label = "show_sdownloadbutton_observe")


      output$download_segment <- downloadHandler(
        filename = function() {
          paste("segment-data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          vroom::vroom_write(x = segment_dataframe(), file = file, delim = ",")
        }
      )
    
      # Show `go to segment summary tab` --------------------------------------|
      observe({
        if (input$assign_manual > 0 || input$assign_agg > 0) {
          shinyjs::show("go_to_segment_summary")
        }
      })
      
      # Switch to segment summary tab -----------------------------------------|
      observe({
        updateNavbarPage(session  = parent_session,
                         inputId  = "navbar_container",
                         selected = "tab_segment_summary")
      }) |>
        bindEvent(input$go_to_segment_summary,
                  label = "go_to_segment_summary_observe")
      
      return(segment_dataframe)
    }
  )
}