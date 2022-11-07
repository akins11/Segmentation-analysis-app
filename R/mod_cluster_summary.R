#' cluster_summary UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_cluster_summary_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      align = "center",

      shiny::column(
        width = 2,

        shiny::tags$div(
          class = "boxIn",

          shinyWidgets::statiCard(
            value = 0,
            subtitle = "Clusters",
            icon = icon("chart-pie"),
            background = cs_col_BC,
            color = "white",
            id = ns("cluster_card")
          )
        ),

      ),

      shiny::column(
        width = 6,

        shinyWidgets::panel(
          class = "panel-color",

          shiny::plotOutput(outputId = ns("cluster_count_plot")) |>
            shinycssloaders::withSpinner(type = 4,
                                         color = spinner_color,
                                         color.background = "white")
        )
      ),

      shiny::column(
        width = 4,

        shinyWidgets::panel(
          class = "panel-color",

          reactable::reactableOutput(outputId = ns("cluster_count_table")) |>
            shinycssloaders::withSpinner(type = 4,
                                         color = spinner_color,
                                         color.background = "white")
        )
      )
    ),

    shiny::tags$br(),

    shiny::uiOutput(outputId = ns("is_with_charcter_var")),

    shiny::tags$br(),

    shiny::fluidRow(
      align = "center",

      shiny::column(
        width = 2,

        shiny::tags$div(
          class = "boxIn",

          shinyWidgets::pickerInput(inputId = ns("num_vars_stat_sumy"),
                                    label   = shiny::tags$h6("Select variable(s)"),
                                    choices = NULL,
                                    multiple = TRUE),

          shiny::tags$br(),

          shinyWidgets::pickerInput(inputId = ns("stat_vars"),
                                    label   = shiny::tags$h6("Aggregate function"),
                                    choices = c("Average" = "mean",
                                                "Median"  = "median",
                                                "Sum"     = "sum",
                                                "Minimum" = "minimum",
                                                ".25 Quantile" = "quantile_25",
                                                ".75 Quantile" = "quantile_75",
                                                "Maximum" = "maximum"),
                                    selected = "mean"),

          shiny::tags$br(),

          shinyWidgets::actionBttn(inputId = ns("run_stat_sumy"),
                                   label = "Run",
                                   style = "stretch",
                                   color = "success")
        )

      ),

      shiny::column(
        width = 6,

        shinyWidgets::panel(
          class = "panel-color",

          plotOutput(outputId = ns("stat_summary_plot")) |>
            shinycssloaders::withSpinner(type = 4,
                                         color = spinner_color,
                                         color.background = "white")
        )
      ),

      shiny::column(
        width = 4,

        shinyWidgets::panel(
          class = "panel-color",

          reactable::reactableOutput(outputId = ns("stat_summary_table")) |>
            shinycssloaders::withSpinner(type = 4,
                                         color = spinner_color,
                                         color.background = "white")
        )
      )
    ),

    shiny::tags$br(),

    shiny::fluidRow(
      align = "center",

      shiny::column(
        width = 2,

        shiny::tags$div(
          class =  "boxIn",

          shinyWidgets::pickerInput(inputId = ns("num_var_x"),
                                    label = tags$h6("X variable"),
                                    choices = NULL),

          shiny::tags$br(),

          shinyWidgets::pickerInput(inputId = ns("num_var_y"),
                                    label = tags$h6("Y variable"),
                                    choices = NULL),

          shiny::tags$br(),

          shinyWidgets::actionBttn(inputId = ns("run_rel_plot"),
                                   label = "Run",
                                   style = "stretch",
                                   color = "success"),

          shiny::tags$br(),
          shiny::tags$br(),

          shinyWidgets::actionBttn(inputId = ns("go_to_assign_segment"),
                                   label = "Assign Segment",
                                   style = "bordered",
                                   color = "success")
        )
      ),

      shiny::column(
        width = 10,

        shinyWidgets::panel(
          class = "panel-color",

          shiny::plotOutput(outputId = ns("cluster_scatter_plot")) |>
            shinycssloaders::withSpinner(type = 4,
                                         color = spinner_color,
                                         color.background = "white")
        )
      )
    )
  )
}




#' cluster_summary Server Functions
#'
#' @param id
#' @param clust_data
#' @param parent_session
#'
#' @noRd
mod_cluster_summary_server <- function(id, clust_data, parent_session) {
  stopifnot(shiny::is.reactive(clust_data))

  shiny::moduleServer(
    id = id,

    module = function(input, output, session) {
    ns <- session$ns

    # Cluster count ---------------------------------------------------------|
    shiny::observe({
      shiny::req(clust_data())

      shinyWidgets::updateStatiCard(
        id = "cluster_card",
        value = length(unique(clust_data()$.cluster))
      )
    }) |>
      shiny::bindEvent(clust_data())

    cluster_count <- shiny::reactive({
      shiny::req(clust_data())

      list(
        table = count_clusters(df = clust_data(), output_type = "table"),
        plot  = count_clusters(df = clust_data(), output_type = "plot")
      )
    }) |>
      shiny::bindEvent(clust_data(), label = "cluster_count_reactive")


    output$cluster_count_table <- reactable::renderReactable({
      clean_reactable_names(x = cluster_count()$table, relocate = FALSE) |>
        summary_reactable(include = "cluster")
    })
    output$cluster_count_plot <- renderPlot({ cluster_count()$plot })



    # Character count -------------------------------------------------------|
    is_char_avaliable <- shiny::reactive({
      is_with_type(df = clust_data(),
                   with_type = "character")
    })

    chr_names <- shiny::reactive({
      shiny::req(clust_data())

      if (isTRUE(is_char_avaliable())) {
        get_var_type_names(df   = clust_data(),
                           type = "character",
                           remove_var = c(".cluster", "row_id"))
      } else {
        character(0)
      }
    }) |>
      shiny::bindEvent(clust_data(), label = "chr_var_names_reactive")

    output$is_with_charcter_var <- shiny::renderUI({
      shiny::req(is_char_avaliable(), chr_names())

      if (isTRUE(is_char_avaliable())) {

        shiny::fluidRow(
          align = "center",

          shiny::column(
            width = 2,

            shiny::tags$div(
              class = c("boxIn", "inputpad"),

              shinyWidgets::pickerInput(inputId = ns("char_var_count"),
                                        label   = shiny::tags$h6("Select a variable"),
                                        choices = chr_names(),
                                        options = shinyWidgets::pickerOptions(title = "Nothing Selected")),

              shiny::tags$br(),

              shiny::numericInput(inputId = ns("chr_count_nrows"),
                                  label   = shiny::tags$h6("Number Of Rows"),
                                  min = 3, max = 100, value = 10)
            )
          ),

          shiny::column(
            width = 6,

            shinyWidgets::panel(
              class = c("panel-color", "panel-h530"),

              shiny::plotOutput(outputId = ns("chr_count_plot"), height = "480px") |>
                shinycssloaders::withSpinner(type = 4,
                                             color = spinner_color,
                                             color.background = "white")
            )
          ),

          shiny::column(
            width = 4,

            shinyWidgets::panel(
              class = "panel-color",

              reactable::reactableOutput(outputId = ns("chr_count_table"))
            )
          )
        )
      }
    })



    char_count_summary <- shiny::reactive({
      shiny::req(clust_data(), input$char_var_count)

      list(
        table = chr_count_cluster(df = clust_data(),
                                  chr_var = input$char_var_count,
                                  output_type = "table"),

        plot = chr_count_cluster(df = clust_data(),
                                 chr_var = input$char_var_count,
                                 output_type = "plot")
      )
    }) |>
      shiny::bindEvent(input$char_var_count, label = "chr_count_sumy_reactive")


    output$chr_count_table <- reactable::renderReactable({
      clean_reactable_names(x = char_count_summary()$table,
                            include = "cluster") |>
        summary_reactable(include = "cluster",
                          page_size = input$chr_count_nrows)
    })
    output$chr_count_plot <- shiny::renderPlot({ char_count_summary()$plot })



    # Cluster stat summary --------------------------------------------------|
    shiny::observe({
      shiny::req(clust_data())

      num_names <- get_var_type_names(df   = clust_data(),
                                      type = "numeric")

      shinyWidgets::updatePickerInput(session = session,
                                      inputId = "num_vars_stat_sumy",
                                      choices = num_names,
                                      options = shinyWidgets::pickerOptions(title = "Nothing Selected"))
    }) |>
      shiny::bindEvent(clust_data(), label = "num_vars_stat_sumy_observe")


    cluster_stat_sumy <- shiny::reactive({
      shiny::req(clust_data(), input$num_vars_stat_sumy, input$stat_vars)

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
      shiny::bindEvent(input$run_stat_sumy, label = "cluster_stat_sumy_reactive")

    output$stat_summary_plot <- shiny::renderPlot({cluster_stat_sumy()$plot })

    output$stat_summary_table <- reactable::renderReactable({
      clean_reactable_names(x = cluster_stat_sumy()$table, relocate = FALSE) |>
        summary_reactable(include = "cluster")
    })

    # Relationship plot -----------------------------------------------------|
    shiny::observe({
      shiny::req(clust_data())

      num_names <- get_var_type_names(df   = clust_data(),
                                      type = "numeric")

      shinyWidgets::updatePickerInput(session = session,
                                      inputId = "num_var_x",
                                      choices = num_names,
                                      selected = character(0),
                                      options = shinyWidgets::pickerOptions(title = "Nothing Selected"))

      shinyWidgets::updatePickerInput(session = session,
                                      inputId = "num_var_y",
                                      choices = num_names,
                                      selected = character(0),
                                      options = shinyWidgets::pickerOptions(title = "Nothing Selected"))

    }) |>
      shiny::bindEvent(clust_data(), label = "rel_plot_observer")

    relationship_plot <- shiny::reactive({
      shiny::req(clust_data(), input$num_var_x, input$num_var_y)

      cluster_relationship_plot(df = clust_data(),
                                num_varx = input$num_var_x,
                                num_vary = input$num_var_y)

    }) |>
      shiny::bindEvent(input$run_rel_plot, label = "rel_plot_reactive")

    output$cluster_scatter_plot <- shiny::renderPlot({ relationship_plot() })



    # Switch to assign segment tab ------------------------------------------|
    shiny::observe({
      shiny::updateNavbarPage(session  = parent_session,
                              inputId  = "navbar_container",
                              selected = "tab_assign_segment")
    }) |>
      shiny::bindEvent(input$go_to_assign_segment,
                       label = "go_to_cluster_summary_observe")
   }
  )
}

