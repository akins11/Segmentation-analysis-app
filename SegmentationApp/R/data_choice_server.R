data_choice_server <- function(id, parent_session) {
  moduleServer(
    id = id,
    
    
    module = function(input, output, session) {
      last_updated_choice <- reactiveValues(val = NULL)
      
      lapply(c("btn_demo_data", "fi_upload_data"),
             function(.x) {
               observe({
                 input[[.x]]
                 last_updated_choice$val <- .x
               })
             })
      
      r_use_data <- reactive(list())
      r_use_data <- reactive({
        if (last_updated_choice$val == "btn_demo_data") {
          req(input$btn_demo_data)
          vroom::vroom("data/demo_data.csv", delim = ",")
          
        } else if (last_updated_choice$val == "fi_upload_data") {
          req(input$fi_upload_data)

          ext <- tools::file_ext(input$fi_upload_data$name)
          switch(ext,
                 csv = vroom::vroom(input$fi_upload_data$datapath, delim = ","),
                 tsv = vroom::vroom(input$fi_upload_data$datapath, delim = "\t"),
                 xls = readxl::read_xls(input$fi_upload_data$datapath),
                 xlsx = readxl::read_xlsx(input$fi_upload_data$datapath),
                 validate("Invalid file; Please upload a cleaned .csv, .xls, .xlsx or .tsv file"))
        }
      })
      
      initial_df_nrow <- reactive({
        req(r_use_data())
        nrow(r_use_data())
      })
      
      has_missing_values <- reactive({
        req(r_use_data())
        check_for_missing_values(r_use_data())
      })
      
      output$drop_missing_value <- renderUI({
        req(r_use_data())
        if (last_updated_choice$val == "fi_upload_data") {
          if (isTRUE(has_missing_values())) {
            tagList(
              chooseSliderSkin("Square"),
              sliderInput(inputId = session$ns("sli_drop_missing_value"),
                          label = tags$h6("Drop variables with selected percentage of missing
                                   values"),
                          min = 0, max = 100, value = 50, step = 5,
                          post = "%", width = "95%")
            )
          }
        }
      })
      
      # Drop missing values ---------------------------------------------------|
      r_data <- reactive({
        req(r_use_data())
        if (isTRUE(has_missing_values())) {
          req(input$sli_drop_missing_value)
          drop_all_missing_values(df = r_use_data(), 
                                  missing_threshold = input$sli_drop_missing_value)
        } else {
          r_use_data()
        }
      })
      
      observe({
        req(r_data(), initial_df_nrow())
        if (!is.data.frame(r_data())) {
          show_alert(
            title = "Missing Values",
            text = "Dropped missing values returned a no data.",
            type = "error",
            width = "50%"
          )
        } else if (is.data.frame(r_data())) {
          percent_rows <- (nrow(r_data()) / initial_df_nrow())*100
          
          if (percent_rows <= 10) {
            if (nrow(r_data()) < 30) {
              v <- if (nrow(r_data()) == 0) c("", "row") else c("just", "rows")
              show_alert(
                title = "Inadiquate Data",
                text = stringr::str_glue("After removing missing values, output returned {v[1]} \\
                                         {nrow(r_data())} {v[2]}."),
                type = "error",
                width = "50%"
              )
            }
          } else if (ncol(r_data()) < 2) {
            v <- if (ncol(r_data()) == 0) c("", "column") else c("just", "columns")
            show_alert(
              title = "Inadiquate variables",
              text = stringr::str_glue("After removing Missing values, output returned {v[1]} \\
                                       {ncol(r_data())} {v[2]}."),
              type = "error",
              width = "50%"
            )
          }
        }
      })
      
   
      lg <- reactive({
        req(initial_df_nrow(), r_data())
        
        check_usable_data_frame(r_data(), initial_df_nrow())
      })
      
      output$drop_missing_value_info <- renderUI({
        if (lg() == FALSE) {
          tagList(
            shinyWidgets::alert(
            "Variables with greater than or equal to 50% (default) missing values
            will be initially removed from the data after which all other missing
            values are removed, this is to make sure that variables with higher
            percentage of missing values do not affect other variables when removing
            missing data by row. You can also increase of decrease the percentage
            thereby changing how missing values are removed. Note that if no value is returned
            then the data is probably containing a large amount of missing values.",
            status = "warning")
          )
        }
      })
      
      # Data Preview ----------------------------------------------------------|
      output$rt_upload_data <- renderReactable({
        req(r_data(), input$ni_n_rows)
        
        if (isTRUE(lg())) {
          clean_reactable_names(x = r_data(), relocate = FALSE) |>
            upload_df_reactable(page_size = input$ni_n_rows)
        }
      })
      
      # Update data info ------------------------------------------------------|
      output$data_info <- renderUI({
        shiny::tagList(
          markdown(get_data_dimension(r_data()))
        )
      })
      
      # un hide `btn_select_vars` btn -----------------------------------------|
      observe({
        req(r_data())
        
        if (isTRUE(lg())) {
          shinyjs::show(id = "btn_select_vars")
        }
      })
      
      output_data <- reactive({
        if (isTRUE(lg())) r_data() else data.frame()
      })
      
      observe({
        req(output_data(), input$btn_select_vars)

        if (nrow(output_data()) == 0 || ncol(output_data()) == 0) {
          show_alert(
            title = "Empty Data Frame",
            text = "Data is empty. Please Check if data upload and cleaning
                   was successful",
            type = "error",
            width = "50%"
          )
        }
      }) |>
        bindEvent(input$btn_select_vars)
      
      # Switch to select variables tab ----------------------------------------|
      observeEvent(input$btn_select_vars, {
        updateNavbarPage(session  = parent_session,
                         inputId  = "navbar_container",
                         selected = "tab_var_selection")
      })
      
      
      return(output_data)
    }
  )
}
# nrow(r_use_data_c()