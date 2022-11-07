#' segmentaion_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_segmentaion_summary_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 3,

        shiny::tags$div(
          class = "boxIn",

          shinyWidgets::pickerInput(inputId = ns("frist_variable"),
                                    label   = shiny::tags$h5("Select a variable"),
                                    choices = NULL),

          shiny::tags$br(),


          shinyWidgets::pickerInput(inputId = ns("second_variable"),
                                    label   = shiny::tags$h5("Select a variable (Optional)"),
                                    choices = NULL),

          shiny::tags$br(),

          shinyjs::hidden(
            shinyWidgets::pickerInput(inputId = ns("aggregate_dash_fun"),
                                      label   = shiny::tags$h6("Aggregate function"),
                                      choices = c("Average" = "mean",
                                                  "Median"  = "median",
                                                  "Sum"     = "sum",
                                                  "Minimum" = "min",
                                                  "Maximum" = "max")),

            shiny::tags$br(),

            shiny::numericInput(inputId = ns("n_category"),
                                label   = shiny::tags$h6("Number of Observations to plot"),
                                min = 5, max = 15, value = 10, step = 1,
                                width = "50%")
          ),

          shiny::tags$br(),

          shinyWidgets::prettyCheckbox(inputId = ns("add_table"),
                                       label  = "Add a table",
                                       value  = FALSE,
                                       status = "primary",
                                       shape  = "square",
                                       animation = "pulse",
                                       bigger = TRUE),

          shiny::tags$br(),

          shiny::numericInput(inputId = ns("n_dash_rows"),
                              label = shiny::tags$h6("Number of rows"),
                              min = 5, max = 100, value = 10, step = 1,
                              width = "50%"),

          shiny::tags$br(),
          shiny::tags$br(),

          shinyWidgets::actionBttn(inputId = ns("create_display"),
                                   label = "Display",
                                   style = "bordered",
                                   color = "success",
                                   size  = "lg")
        )
      ),

      shiny::column(
        width = 9,

        shinyWidgets::panel(
          class = "panel-color",

          shiny::plotOutput(outputId = ns("plot_output")) |>
            shinycssloaders::withSpinner(type = 4,
                                         color = spinner_color,
                                         color.background = "white"),

          shiny::tags$br(),
          shiny::tags$br(),

          shiny::plotOutput(outputId = ns("t_plot_output")),
          reactable::reactableOutput(outputId = ns("p_table_output")),

          shiny::tags$br(),
          shiny::tags$br(),

          reactable::reactableOutput(outputId = ns("table_output")) |>
            shinycssloaders::withSpinner(type = 4,
                                         color = spinner_color,
                                         color.background = "white")
        )
      )
    )
  )
}


#' segmentaion_summary Server Functions
#'
#' @param id
#' @param seg_data
#'
#' @noRd
mod_segmentaion_summary_server <- function(id, seg_data) {
  stopifnot(shiny::is.reactive(seg_data))

  shiny::moduleServer(
    id = id,

    module = function(input, output, session) {

    ns <- session$ns

    # add variables to ui ---------------------------------------------------|
    shiny::observe({
      shiny::req(seg_data())

      var_names <- names(seg_data())
      var_names <- var_names[which(!var_names %in% c(".cluster", ".segment"))]

      shinyWidgets::updatePickerInput(session  = session,
                                      inputId  = "frist_variable",
                                      choices  = var_names,
                                      options  = shinyWidgets::pickerOptions(title = "Nothing Selected"))

      shinyWidgets::updatePickerInput(session  = session,
                                      inputId  = "second_variable",
                                      choices  = c(var_names, "No Selection"),
                                      selected = "No Selection")
    })


    # create validation -----------------------------------------------------|
    shiny::observe({
      if (nchar(input$frist_variable) == 0) {
        shinyWidgets::show_alert(
          title = "Error !!",
          text = "The first selection can not be empty.",
          type = "error"
        )
      }
    }) |>
      shiny::bindEvent(input$create_display)


    # Necessary Inputs ------------------------------------------------------|
    shiny::observe({
      shiny::req(input$frist_variable, input$second_variable)

      if (input$frist_variable != "" && input$second_variable != "No Selection") {
        selected_dtype <- unite_dtypes(seg_data(),
                                       input$frist_variable,
                                       input$second_variable) |> as.vector()

        if (all(c("numeric", "character") %in% selected_dtype)) {
          shinyjs::show(id = "aggregate_dash_fun", anim = TRUE)
        } else {
          shinyjs::hide(id = "aggregate_dash_fun", anim = TRUE)
        }
      }
    }) |>
      shiny::bindEvent(input$frist_variable, input$second_variable)

    shiny::observe({
      shiny::req(input$frist_variable, input$second_variable)

      if (input$frist_variable != "" && input$second_variable != "No Selection") {
        selected_dtype <- unite_dtypes(seg_data(),
                                       input$frist_variable,
                                       input$second_variable) |> as.vector()

        if ("character" %in% selected_dtype) {
          shinyjs::show(id = "n_category", anim = TRUE)
        } else {
          shinyjs::hide(id = "n_category", anim = TRUE)
        }
      } else if (input$frist_variable != "" && input$second_variable == "No Selection") {
        selected_dtype <- unite_dtypes(seg_data(), input$frist_variable)

        if (selected_dtype == "character") {
          shinyjs::show(id = "n_category", anim = TRUE)
        } else {
          shinyjs::hide(id = "n_category", anim = TRUE)
        }
      }
    })|>
      shiny::bindEvent(input$frist_variable, input$second_variable)


    # display functions -----------------------------------------------------|
    output_list <- shiny::reactive({
      shiny::req(seg_data(), input$frist_variable, input$second_variable,
          input$n_category, input$aggregate_dash_fun)

      num_chr_wrapper(wr_df   = seg_data(),
                      var_one = input$frist_variable,
                      var_two = input$second_variable,
                      n_cat   = input$n_category,
                      str_fun = input$aggregate_dash_fun)
    }) |>
      shiny::bindEvent(input$create_display, label = "output_list_reactive")


    # output options --------------------------------------------------------|
    output$plot_output <- shiny::renderPlot({
      output_list()[[1]]
    })

    output$t_plot_output <- shiny::renderPlot({
      if (length(output_list()) == 3) {
        output_list()[[2]]
      }
    })

    output$p_table_output <- reactable::renderReactable({
      shiny::req(output_list())

      if (length(output_list()) == 2) {
        clean_reactable_names(x = output_list()[[2]]) |>
          summary_reactable(include = "segment",
                            page_size = input$n_dash_rows)
      }
    })

    shiny::observe({
      if (is.data.frame(output_list()[[2]])) {
        shinyjs::hide(id = "t_plot_output")
        shinyjs::show(id = "p_table_output")

      } else {
        shinyjs::show(id = "t_plot_output")
        shinyjs::hide(id = "p_table_output")
      }
    })


    create_table <- shiny::reactive({
      if (input$add_table  && length(output_list()) == 3) {
        output_list()[[3]]
      }
    }) |>
      shiny::bindEvent(input$create_display)


    output$table_output <- reactable::renderReactable({
      shiny::req(create_table())

      clean_reactable_names(x = create_table()) |>
        summary_reactable(include = "segment",
                          page_size = input$n_dash_rows)
    })
  }
 )
}

