#' select_variables UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_select_variables_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      align = "center",

      shiny::column(
        width = 3,
        shiny::tags$br(),

        shiny::tags$div(
          class = c("boxIn", "inputpad"),

          shiny::tags$h6(id = "vars-id",  "Select variables for cluster analysis"),

          shiny::tags$p(
            id = "select-vars-info",
            "
           A minimum of two variable must be selected.
           Numeric variables are more preferred than character variables
           and also Fewer variables are best for computational performance.
           "
          ),
          shinyWidgets::pickerInput(inputId = ns("select_cluster_vars"),
                                    label = "",
                                    choices = NULL,
                                    multiple = TRUE,
                                    options = list(`actions-box` = TRUE,
                                                   `select-all-text` = "Select All",
                                                   `none-selected-text` = "No Selection Yet",
                                                   `multiple-separator` = " | ")),

          shiny::tags$br(),

          shinyWidgets::pickerInput(inputId  = ns("select_data_id"),
                                    label    = h6("Select an ID variable (if avaliable)"),
                                    choices  = NULL,
                                    selected = "No Selection",
                                    multiple = FALSE),

          shiny::tags$br(),

          shiny::numericInput(inputId = ns("ni_n_sv_rows"),
                              label   = h6("Number of rows"),
                              min     = 3, max = 100, value = 5, step = 1,
                              width = "60%"),

          shiny::tags$br(),

          shinyWidgets::prettyToggle(inputId = ns("pt_stand_vars"),
                                     label_on = "Standardize",
                                     label_off = "Don't standardize",
                                     value = TRUE,
                                     status_on = "success",
                                     status_off = "warning",
                                     bigger = TRUE),

          shiny::tags$br(),
          shiny::tags$br(),

          shinyWidgets::actionBttn(inputId = ns("btn_go_to_cluster_algo"),
                                   label = "Run Cluster Analysis",
                                   style = "bordered",
                                   color = "success") |>
            shinyjs::hidden()
        )
      ),

      shiny::column(
        width = 9,

        shiny::tags$br(),

        shinyWidgets::panel(
          class = "panel-color",
          shiny::tags$h6("Selected Variables"),

          reactable::reactableOutput(outputId = ns("selected_vars_out")) |>
            shinycssloaders::withSpinner(type = 4,
                                         color = spinner_color,
                                         color.background = "white")
        ),

        shiny::tags$br(),

        shinyWidgets::panel(
          class = "panel-color",
          shiny::tags$h6("Restructured Data"),

          reactable::reactableOutput(outputId = ns("engineered_data")) |>
            shinycssloaders::withSpinner(type = 4,
                                         color = spinner_color,
                                         color.background = "white")
        )
      )
    )
  )
}




#' select_variables Server Functions
#'
#' @param id
#' @param r_use_data
#' @param parent_session
#'
#' @noRd
mod_select_variables_server <- function(id, r_use_data, parent_session) {
  stopifnot(shiny::is.reactive(r_use_data))

  shiny::moduleServer(
    id = id,

    module = function(input, output, session) {
    ns <- session$ns

    # Update variable selection ---------------------------------------------|
    shiny::observe({
      shiny::req(r_use_data())

      shinyWidgets::updatePickerInput(session = session,
                                      inputId = "select_cluster_vars",
                                      choices = names(r_use_data()))
    }) |>
      shiny::bindEvent(r_use_data())

    shiny::observe({
      shiny::req(input$select_cluster_vars)

      shinyWidgets::updatePickerInput(session = session,
                                      inputId = "select_data_id",
                                      choices = c(input$select_cluster_vars, "No Selection"),
                                      selected = "No Selection")
    }) |>
      shiny::bindEvent(input$select_cluster_vars)

    # selecting variables ---------------------------------------------------|
    selected_vars <- shiny::reactive({
      shiny::req(r_use_data(), input$select_cluster_vars, input$ni_n_sv_rows)

      select_input_vars(df = r_use_data(),
                        variables = input$select_cluster_vars)
    })

    # Data cleaning/feature engineering -------------------------------------|
    clean_data <- shiny::reactive({
      shiny::req(selected_vars(), input$select_data_id)


      push_to_cluster_alg(df = selected_vars(),
                          id_var = input$select_data_id,
                          standardize = input$pt_stand_vars)
    })

    # Dropping id variable --------------------------------------------------|
    r_use_data_f <- shiny::reactive({
      shiny::req(r_use_data(), input$select_data_id)

      drop_data_id(df = r_use_data(), id_var = input$select_data_id)
    })

    # Data preview ----------------------------------------------------------|
    output$selected_vars_out <- reactable::renderReactable({
      clean_reactable_names(x = selected_vars(), relocate = FALSE) |>
        var_selection_restructure_rt(page_size = input$ni_n_sv_rows)
    })
    output$engineered_data <- reactable::renderReactable({
      clean_reactable_names(x = clean_data(), relocate = FALSE) |>
        var_selection_restructure_rt(page_size = input$ni_n_sv_rows)
    })

    # Show go to `go to cluster algorithm` panel ----------------------------|
    shiny::observe({
      shiny::req(input$select_cluster_vars, clean_data())

      shinyjs::show(id = "btn_go_to_cluster_algo")
    })

    # Switch to run cluster analysis tab ------------------------------------|
    shiny::observeEvent(input$btn_go_to_cluster_algo, {
      shiny::updateNavbarPage(session  = parent_session,
                              inputId  = "navbar_container",
                              selected = "tab_run_cluster")
    })

    # Return data frames ----------------------------------------------------|
    dataframe <- shiny::reactive({
      list(original_table   = r_use_data_f(),
           engineered_table = clean_data())
    })

    return(dataframe)
   }
  )
}
