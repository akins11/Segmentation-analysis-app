select_vars_server <- function(id, r_use_data, parent_session) {
  stopifnot(is.reactive(r_use_data))
  
  moduleServer(
    id = id,
    
    module = function(input, output, session) {
      # Update variable selection ---------------------------------------------|
      observe({
        req(r_use_data())
        
        updatePickerInput(session = session,
                          inputId = "select_cluster_vars",
                          choices = names(r_use_data()))
      }) |>
        bindEvent(r_use_data())
      
      observe({
        req(input$select_cluster_vars)
        
        updatePickerInput(session = session,
                          inputId = "select_data_id",
                          choices = c(input$select_cluster_vars, "No Selection"),
                          selected = "No Selection")
      }) |>
        bindEvent(input$select_cluster_vars)
      
      # selecting variables ---------------------------------------------------|
      selected_vars <- reactive({
        req(r_use_data(), input$select_cluster_vars, input$ni_n_sv_rows)
        
        select_input_vars(df = r_use_data(), 
                          variables = input$select_cluster_vars)
      })
      
      # Data cleaning/feature engineering -------------------------------------|
      clean_data <- reactive({
        req(selected_vars(), input$select_data_id)
        
        
        push_to_cluster_alg(df = selected_vars(), 
                            id_var = input$select_data_id,
                            standardize = input$pt_stand_vars)
      })
      
      # Dropping id variable --------------------------------------------------|
      r_use_data_f <- reactive({
        req(r_use_data(), input$select_data_id)
        
        drop_data_id(df = r_use_data(), id_var = input$select_data_id)
      })
      
      # Data preview ----------------------------------------------------------|
      output$selected_vars_out <- renderReactable({
        clean_reactable_names(x = selected_vars(), relocate = FALSE) |>
          var_selection_restructure_rt(page_size = input$ni_n_sv_rows)
      })
      output$engineered_data <- renderReactable({
        clean_reactable_names(x = clean_data(), relocate = FALSE) |>
          var_selection_restructure_rt(page_size = input$ni_n_sv_rows)
      })
      
      # Show go to `go to cluster algorithm` panel ----------------------------|
      observe({
        req(input$select_cluster_vars, clean_data())
        
        shinyjs::show(id = "btn_go_to_cluster_algo")
      })
      
      # Switch to run cluster analysis tab ------------------------------------|
      observeEvent(input$btn_go_to_cluster_algo, {
        updateNavbarPage(session  = parent_session,
                         inputId  = "navbar_container",
                         selected = "tab_run_cluster")
      })
      
      # Return data frames ----------------------------------------------------|
      dataframe <- reactive({
        list(original_table   = r_use_data_f(),
             engineered_table = clean_data())
      })
      
      return(dataframe)
    }
  )
}