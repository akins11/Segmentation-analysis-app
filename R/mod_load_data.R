#' load_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_load_data_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      align = "center",
      tags$br(),

      shiny::column(
        width = 2,

        div(
          class = c("boxIn", "inputMag"),

          shinyWidgets::dropMenu(tag = shinyWidgets::actionBttn(inputId = ns("btn_upload_data"),
                                                                label = "Upload Data",
                                                                style = "stretch",
                                                                color = "primary"),

                   shiny::fileInput(inputId = ns("fi_upload_data"),
                                    label  = "",
                                    accept = c(".csv", ".tsv"))
          ),

          shiny::tags$br(),

          shinyWidgets::actionBttn(inputId = ns("btn_demo_data"),
                                   label = "Use Demo Data",
                                   style = "stretch",
                                   color = "primary",
                                   size = "sm"),

          shiny::tags$br(),
          shiny::tags$br(),

          shiny::uiOutput(outputId = ns("drop_missing_value")),

          shiny::tags$br(),

          shiny::numericInput(inputId = ns("ni_n_rows"),
                              label = shiny::tags$h6("Number Of rows"),
                              min   = 3, max = 100, step = 1, value = 10,
                              width = "180px"),

          shiny::tags$br(),
          shiny::tags$br(),

          shinyjs::hidden(
            shinyWidgets::actionBttn(inputId = ns("btn_select_vars"),
                                     label = "Select Variables",
                                     style = "bordered",
                                     color = "success")
          )
        )
      ),

      shiny::column(
        width = 10,

        shinyWidgets::panel(
          class = "panel-color",
          shiny::tags$h5("Data Preview"),
          shiny::tags$br(),

          reactable::reactableOutput(outputId = ns("rt_upload_data")) |>
            shinycssloaders::withSpinner(type = 4,
                                         color = spinner_color,
                                         color.background = "white")
        ),

        shinyWidgets::panel(
          class = c("missing-info", "panel-color"),

          reactable::reactableOutput(outputId = ns("data_info")),

          shiny::tags$br(),

          shiny::uiOutput(outputId = ns("drop_missing_value_info"))
        )
      )
    )
  )
}







#' load_data Server Functions
#'
#' @param id
#' @param parent_session
#'
#' @noRd
mod_load_data_server <- function(id, parent_session) {

  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    last_updated_choice <- shiny::reactiveValues(val = NULL)

    lapply(c("btn_demo_data", "fi_upload_data"),
           function(.x) {
             shiny::observe({
               input[[.x]]
               last_updated_choice$val <- .x
             })
           })

    r_use_data <- shiny::reactive(list())
    r_use_data <- shiny::reactive({
      if (last_updated_choice$val == "btn_demo_data") {
        shiny::req(input$btn_demo_data)
        vroom::vroom("data/demo_data.csv", delim = ",")

      } else if (last_updated_choice$val == "fi_upload_data") {
        shiny::req(input$fi_upload_data)

        ext <- tools::file_ext(input$fi_upload_data$name)
        switch(ext,
               csv = vroom::vroom(input$fi_upload_data$datapath, delim = ","),
               tsv = vroom::vroom(input$fi_upload_data$datapath, delim = "\t"),
               xls = readxl::read_xls(input$fi_upload_data$datapath),
               xlsx = readxl::read_xlsx(input$fi_upload_data$datapath),
               shiny::validate("Invalid file; Please upload a cleaned .csv, .xls, .xlsx or .tsv file"))
      }
    })

    initial_df_nrow <- shiny::reactive({
      shiny::req(r_use_data())
      nrow(r_use_data())
    })

    has_missing_values <- shiny::reactive({
      shiny::req(r_use_data())
      check_for_missing_values(r_use_data())
    })

    output$drop_missing_value <- shiny::renderUI({
      shiny::req(r_use_data())
      if (last_updated_choice$val == "fi_upload_data") {
        if (isTRUE(has_missing_values())) {
          shiny::tagList(
            shinyWidgets::chooseSliderSkin("Square"),
            shiny::sliderInput(inputId = ns("sli_drop_missing_value"),
                               label = shiny::tags$h6("Drop variables with selected percentage of missing values"),
                               min = 0, max = 100, value = 50, step = 5,
                               post = "%", width = "95%")
          )
        }
      }
    })

    # Drop missing values ---------------------------------------------------|
    r_data <- shiny::reactive({
      shiny::req(r_use_data())
      if (isTRUE(has_missing_values())) {
        shiny::req(input$sli_drop_missing_value)
        drop_all_missing_values(df = r_use_data(),
                                missing_threshold = input$sli_drop_missing_value)
      } else {
        r_use_data()
      }
    })

    shiny::observe({
      shiny::req(r_data(), initial_df_nrow())

      if (!is.data.frame(r_data())) {
        shinyWidgets::show_alert(
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
            shinyWidgets::show_alert(
              title = "Inadiquate Data",
              text = stringr::str_glue("After removing missing values, output returned {v[1]} \\
                                         {nrow(r_data())} {v[2]}."),
              type = "error",
              width = "50%"
            )
          }

        } else if (ncol(r_data()) < 2) {
          v <- if (ncol(r_data()) == 0) c("", "column") else c("just", "columns")
          shinyWidgets::show_alert(
            title = "Inadiquate variables",
            text = stringr::str_glue("After removing Missing values, output returned {v[1]} \\
                                       {ncol(r_data())} {v[2]}."),
            type = "error",
            width = "50%"
          )
        }
      }
    })


    lg <- shiny::reactive({
      shiny::req(initial_df_nrow(), r_data())

      check_usable_data_frame(r_data(), initial_df_nrow())
    })

    output$drop_missing_value_info <- shiny::renderUI({
      if (lg() == FALSE) {
        shiny::tagList(
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
    output$rt_upload_data <- reactable::renderReactable({
      shiny::req(r_data(), input$ni_n_rows)

      if (isTRUE(lg())) {
        clean_reactable_names(x = r_data(), relocate = FALSE) |>
          upload_df_reactable(page_size = input$ni_n_rows)
      }
    })

    # Update data info ------------------------------------------------------|
    output$data_info <- reactable::renderReactable({
      shiny::req(r_data())
      get_data_dimension(r_data())
    })

    # un hide `btn_select_vars` btn -----------------------------------------|
    shiny::observe({
      shiny::req(r_data())

      if (isTRUE(lg())) {
        shinyjs::show(id = "btn_select_vars")
      }
    })

    output_data <- shiny::reactive({
      if (isTRUE(lg())) r_data() else data.frame()
    })

    shiny::observe({
      shiny::req(output_data(), input$btn_select_vars)

      if (nrow(output_data()) == 0 || ncol(output_data()) == 0) {
        shinyWidgets::show_alert(
          title = "Empty Data Frame",
          text = "Data is empty. Please Check if data upload and cleaning
                   was successful",
          type = "error",
          width = "50%"
        )
      }
    }) |>
      shiny::bindEvent(input$btn_select_vars)

    # Switch to select variables tab ----------------------------------------|
    shiny::observeEvent(input$btn_select_vars, {
      shiny::updateNavbarPage(session  = parent_session,
                              inputId  = "navbar_container",
                              selected = "tab_var_selection")
    })


    return(output_data)
  }
 )
}
