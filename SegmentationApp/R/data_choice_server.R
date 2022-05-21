data_choice_server <- function(id, parent_session) {
  moduleServer(
    id = id,
    
    module = function(input, output, session) {
      r_use_data <- reactive(list())
      r_use_data <- reactive({

        if(input$btn_demo_data) {
          req(input$btn_demo_data)

          vroom::vroom("data/demo_data.csv", delim = ",")

        } else {
          req(input$fi_upload_data)

          ext <- tools::file_ext(input$fi_upload_data$name)
          switch(ext,
                 csv = vroom::vroom(input$fi_upload_data$datapath, delim = ","),
                 tsv = vroom::vroom(input$fi_upload_data$datapath, delim = "\t"),
                 validate("Invalid file; Please upload a .csv or .tsv file"))
        }
      })
      
      observe({
        if (is.data.frame(r_use_data()) && input$btn_demo_data > 0)
        disableDropMenu(id = "btn_upload_data_dropmenu")
      }) 

      observe({
        if (input$btn_upload_data_dropmenu == TRUE && 
            input$btn_demo_data == 0 && is.data.frame(r_use_data())) {
          shinyjs::hide(id = "btn_demo_data")
        }
      }) 
      
   
      # Data Preview ----------------------------------------------------------|
      output$rt_upload_data <- renderReactable({
        req(r_use_data(), input$ni_n_rows)
        
        clean_reactable_names(x = r_use_data(), relocate = FALSE) |>
          upload_df_reactable(page_size = input$ni_n_rows)
      })
      
      
      # un hide `btn_select_vars` btn -----------------------------------------|
      observe({
        req(r_use_data())

        shinyjs::show(id = "btn_select_vars")
      })
      
      
      # Switch to select variables tab ----------------------------------------|
      observeEvent(input$btn_select_vars, {
        updateNavbarPage(session  = parent_session,
                         inputId  = "navbar_container",
                         selected = "tab_var_selection")
      })
      
      return(r_use_data)
    }
  )
}