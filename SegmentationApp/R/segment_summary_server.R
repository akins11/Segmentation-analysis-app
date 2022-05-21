segment_summary_server <- function(id, seg_data) {
  stopifnot(is.reactive(seg_data))
  
  moduleServer(
    id = id,
    
    module = function(input, output, session) {
      # add variables to ui ---------------------------------------------------|
      observe({
        req(seg_data())
        
        var_names <- names(seg_data())
        var_names <- var_names[which(!var_names %in% c(".cluster", ".segment"))]
        
        updatePickerInput(session  = session,
                          inputId  = "frist_variable",
                          choices  = var_names,
                          options  = pickerOptions(title = "Nothing Selected"))
        
        updatePickerInput(session  = session,
                          inputId  = "second_variable",
                          choices  = c(var_names, "No Selection"),
                          selected = "No Selection")
      })
      
      
      # create validation -----------------------------------------------------|
      observe({
        if (nchar(input$frist_variable) == 0) {
          show_alert(
            title = "Error !!",
            text = "The first Selection can not be empty.",
            type = "error"
          )
        }
      }) |>
        bindEvent(input$create_display)
      
      
      # Necessary Inputs ------------------------------------------------------|
      observe({
        req(input$frist_variable, input$second_variable)
        
        if (input$frist_variable != "" && input$second_variable != "No Selection") {
          selected_dtype <- unite_dtypes(seg_data(),
                                         input$frist_variable,
                                         input$second_variable) |> as.vector()
          
          if (all(c("numeric", "character") %in% selected_dtype)) {
            shinyjs::show(id = "aggregate_dash_fun")
          } else {
            shinyjs::hide(id = "aggregate_dash_fun")
          }
        }
      }) |>
        bindEvent(input$frist_variable, input$second_variable)
      
      observe({
        req(input$frist_variable, input$second_variable)
        
        if (input$frist_variable != "" && input$second_variable != "No Selection") {
          selected_dtype <- unite_dtypes(seg_data(),
                                         input$frist_variable,
                                         input$second_variable) |> as.vector()
          
          if ("character" %in% selected_dtype) {
            shinyjs::show(id = "n_category")
          } else {
            shinyjs::hide(id = "n_category")
          }
        } else if (input$frist_variable != "" && input$second_variable == "No Selection") {
          selected_dtype <- unite_dtypes(seg_data(), input$frist_variable)
          
          if (selected_dtype == "character") {
            shinyjs::show(id = "n_category")
          } else {
            shinyjs::hide(id = "n_category")
          }
        }
      })|>
        bindEvent(input$frist_variable, input$second_variable)
      
      
      # display functions -----------------------------------------------------|
      output_list <- reactive({
        req(seg_data(), input$frist_variable, input$second_variable, 
            input$n_category, input$aggregate_dash_fun)
        
        num_chr_wrapper(wr_df   = seg_data(),
                        var_one = input$frist_variable,
                        var_two = input$second_variable,
                        n_cat   = input$n_category,
                        str_fun = input$aggregate_dash_fun)
      }) |>
        bindEvent(input$create_display, label = "ouput_list_reactive")
      
      
      # output options --------------------------------------------------------|
      output$plot_output <- renderPlot({
        output_list()[[1]]
      })
      
      output$t_plot_output <- renderPlot({
        if (length(output_list()) == 3) {
          output_list()[[2]]
        }
      })
  
      output$p_table_output <- renderReactable({
        req(output_list())
        
        if (length(output_list()) == 2) {
          clean_reactable_names(x = output_list()[[2]]) |>
            summary_reactable(include = "segment",
                              page_size = input$n_dash_rows)
        }
      })
      
      observe({
        if (is.data.frame(output_list()[[2]])) {
          shinyjs::hide(id = "t_plot_output")
          shinyjs::show(id = "p_table_output")
          
        } else {
          shinyjs::show(id = "t_plot_output")
          shinyjs::hide(id = "p_table_output")
        }
      })
      
      
      create_table <- reactive({
        if (input$add_table  && length(output_list()) == 3) {
          output_list()[[3]]
        }
      }) |>
        bindEvent(input$create_display)
      
       
      output$table_output <- renderReactable({
        req(create_table())
        
        clean_reactable_names(x = create_table()) |>
          summary_reactable(include = "segment",
                            page_size = input$n_dash_rows)
      })
    }
  )
}