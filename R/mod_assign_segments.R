#' assign_segments UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_assign_segments_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 3,
        align = "center",

        shiny::tags$div(
          class = "boxIn",

          shiny::tags$h5("Method Of Assignment"),
          shiny::tags$br(),

          shinyWidgets::dropMenu(
            tag =  shinyWidgets::actionBttn(inputId = ns("manual_assginment_method"),
                                            label = "Manual Assignment",
                                            style = "stretch",
                                            color = "primary",
                                            size  = "md"),


            shiny::uiOutput(outputId = ns("segment_values")),

            shiny::tags$br(),
            shiny::tags$br(),

            shinyWidgets::prettyCheckbox(inputId = ns("manual_drop_cluster"),
                                         label  = "Drop Cluster Variable",
                                         shape  = "square",
                                         status = "danger",
                                         bigger = TRUE),

            shiny::tags$br(),
            shiny::tags$br(),

            shiny::actionButton(inputId = ns("assign_manual"),
                                label = "Assign",
                                class = "monitor")
          ),

          tags$br(),

          shinyWidgets::dropMenu(
            tag = shinyWidgets::actionBttn(inputId = ns("aggregate_assignment_method"),
                                           label = "Aggregate Assignment",
                                           style = "stretch",
                                           color = "primary",
                                           size  = "md"),

            shinyWidgets::actionBttn(inputId = ns("show_agg_info"),
                                     label = "More Info",
                                     style = "material-circle",
                                     icon  = icon("info-circle")),

            shiny::tags$br(),

            shinyWidgets::pickerInput(inputId = ns("agg_numeric_variable"),
                                      label = shiny::tags$h4("Select a variable"),
                                      choices = NULL),

            shiny::tags$br(),
            shiny::tags$h4("Segments"),
            shiny::textInput(inputId = ns("agg_segments_value"),
                             label = "values must be separated with comma",
                             value = "",
                             placeholder = "Segments...."),

            shiny::tags$br(),

            shinyWidgets::pickerInput(inputId = ns("agg_function"),
                                      label = shiny::tags$h4("Use Function"),
                                      choices = c("Average" = "mean",
                                                  "Median"  = "median",
                                                  "Sum"     = "sum",
                                                  "Minimum" = "min",
                                                  "Maximum" = "max"),
                                      selected = "mean"),

            shiny::tags$br(),

            shinyWidgets::prettyCheckbox(inputId = ns("agg_drop_cluster"),
                                         label  = "Drop Cluster Variable",
                                         shape  = "square",
                                         status = "danger",
                                         bigger = TRUE),

            shiny::tags$br(),

            shiny::actionButton(inputId = ns("assign_agg"),
                                label = "Assign",
                                class = "monitor")
          ),

          shiny::tags$br(),
          shiny::tags$br(),

          shinyWidgets::actionBttn(inputId = ns("go_to_segment_summary"),
                                   label = "Segmentation Summary",
                                   style = "bordered",
                                   color = "success") |>
            shinyjs::hidden()
        )
      ),

      column(
        width = 9,

        shinyWidgets::panel(
          class = "panel-color",

          reactable::reactableOutput(outputId = ns("segment_table")) |>
            shinycssloaders::withSpinner(type = 4,
                                         color = spinner_color,
                                         color.background = "white")
        ),

        shiny::tags$br(),

        shiny::tags$div(
          class = "center-div",

          shiny::downloadButton(outputId = ns("download_segment"),
                                label = "Download Data .csv") |>
            shinyjs::hidden()
        )
      )
    )
  )
}



#' assign_segments Server Functions
#'
#' @param id
#' @param clust_data
#' @param parent_session
#'
#' @noRd
mod_assign_segments_server <- function(id, clust_data, parent_session) {
  stopifnot(shiny::is.reactive(clust_data))

  shiny::moduleServer(
    id = id,

    module = function(input, output, session) {

    ns <- session$ns

    # Manual Assignment -----------------------------------------------------|
    num_clusters <- shiny::reactive({
      shiny::req(clust_data())
      get_num_clusters(df = clust_data())
    })

    output$segment_values <- shiny::renderUI({
      shiny::req(num_clusters())
      lapply(seq_len(num_clusters()), function(.x) {
        shinyWidgets::textInputIcon(inputId = ns(paste0("seg_", .x)),
                                    label = "",
                                    icon  = list(paste("cluster", .x)),
                                    value = "")
      })
    })

    combine_segments <- shiny::reactive({
      shiny::req(clust_data())
      cs <- c()
      for (i in seq_len(num_clusters())) {
        cs <- c(cs, input[[paste0("seg_", i)]])
      }
      cs
    })

    manual_seg_assigment <- shiny::reactive({
      shiny::req(clust_data(), combine_segments())

      if (input$assign_manual > 0) {
        assign_seg_manually(df = clust_data(),
                            segment = combine_segments(),
                            drop_cluster = input$manual_drop_cluster)
      }

    }) |>
      shiny::bindEvent(input$assign_manual, label = "manual_seg_reactive")



    # Aggregate Assignment --------------------------------------------------|

    shiny::observe({
      shinyWidgets::show_toast(
        title = "Aggregate Summary Info",
        text = "Select a numerical variable which will be summarise using the choosen
               aggregate function, The calculation will be done for each cluster and
               the result will be sorted from the largest to the smallest.
              ",
        type = "info",
        timer = 14000,
        position = "center",
        width = 550
      )
    }) |>
      shiny::bindEvent(input$show_agg_info)



    shiny::observe({
      numeric_vars_names <- get_var_type_names(df = clust_data(),
                                               type = "numeric")

      shinyWidgets::updatePickerInput(session = session,
                                      inputId = "agg_numeric_variable",
                                      choices = numeric_vars_names,
                                      options = shinyWidgets::pickerOptions(title = "Nothing Selected"))
    })

    user_supplied_segments <- shiny::reactive({
      shiny::req(input$agg_segments_value)
      get_user_segments(input$agg_segments_value)
    })

    shiny::observe({
      shiny::req(num_clusters(), user_supplied_segments())

      if (num_clusters() != length(user_supplied_segments())) {
        if (num_clusters() > length(user_supplied_segments())) {
          shinyWidgets::show_alert(
            title = "Wrong Number Of Segments",
            text = paste("There are", num_clusters(), "avaliable",
                         "clusters but you only supplied",
                         length(user_supplied_segments()),
                         "segments. Note that the number of segments
                          supplied must be the same number of avaliable
                          clusters"),
            type = "error")

        } else if (num_clusters() < length(user_supplied_segments())) {
          shinyWidgets::show_alert(
            title = "Wrong Number Of Segments",
            text = paste("There are only", num_clusters(), "avaliable",
                         "clusters but you supplied",
                         length(user_supplied_segments()),
                         "segments. Note that the number of segments
                         supplied must be the same number of avaliable
                         clusters"),
            type = "error")
        }
      }
    }) |>
      shiny::bindEvent(input$assign_agg)

    shiny::observe({
      shiny::req(clust_data())
      if (input$agg_numeric_variable == "") {
        shinyWidgets::show_alert(title = "No Variable Was Selected",
                                 text = "A numerical variable must be chosen",
                                 type = "error")
      }
    }) |>
      shiny::bindEvent(input$assign_agg)

    agg_seg_assignment <- shiny::reactive({
      shiny::req(clust_data(), user_supplied_segments(), input$agg_numeric_variable,
                 input$agg_function)

      if (input$assign_agg > 0 ) {
        assign_seg_num_var(df = clust_data(),
                           num_var = input$agg_numeric_variable,
                           str_fun = input$agg_function,
                           segment = user_supplied_segments(),
                           drop_cluster = input$agg_drop_cluster)
      }
    }) |>
      shiny::bindEvent(input$assign_agg, label = "agg_seg_reactive")



    # segment data ----------------------------------------------------------|
    segment_dataframe <- shiny::reactive({
      # req(input$last_click)
      if (input$assign_manual > 0 &&
          input$last_click == "assign_segments-assign_manual") {
        manual_seg_assigment()

      } else if (input$assign_agg > 0 &&
                 input$last_click == "assign_segments-assign_agg") {
        agg_seg_assignment()
      }
    })

    output$segment_table <- reactable::renderReactable({
      shiny::req(segment_dataframe())

      clean_reactable_names(x = segment_dataframe(),
                            relocate = TRUE,
                            include = "segment") |>
        cluster_segment_reactable(include = "segment", page_size = 20)
    })

    # Download segment data -------------------------------------------------|
    shiny::observe({
      shinyjs::show(id = "download_segment")
    }) |>
      shiny::bindEvent(segment_dataframe(), label = "show_sdownloadbutton_observe")


    output$download_segment <- shiny::downloadHandler(
      filename = function() {
        paste("segment-data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        vroom::vroom_write(x = segment_dataframe(), file = file, delim = ",")
      }
    )

    # Show `go to segment summary tab` --------------------------------------|
    shiny::observe({
      if (input$assign_manual > 0 || input$assign_agg > 0) {
        shinyjs::show("go_to_segment_summary")
      }
    })

    # Switch to segment summary tab -----------------------------------------|
    shiny::observe({
      shiny::updateNavbarPage(session  = parent_session,
                              inputId  = "navbar_container",
                              selected = "tab_segment_summary")
    }) |>
      shiny::bindEvent(input$go_to_segment_summary,
                       label = "go_to_segment_summary_observe")

    return(segment_dataframe)
  }
 )
}

