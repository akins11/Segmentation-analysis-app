#' run_cluster_analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_run_cluster_analysis_ui <- function(id) {

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::tags$br(),

      shiny::column(
        width = 3,

        shiny::tags$div(
          class = "boxIn",

          shinyWidgets::dropMenu(
            tag = shinyWidgets::prettyRadioButtons(inputId = ns("algorithm_choice"),
                                                   label = tags$h5("Select an algorithm"),
                                                   choices = c("K-partition clustering (kmeans)",
                                                               "Hierarchical clustering"),
                                                   selected = character(0),
                                                   shape = "square",
                                                   animation = "pulse",
                                                   bigger = TRUE),

            shiny::selectInput(inputId = ns("more_algorithm_choice"),
                               label = shiny::tags$h5("More cluester algorithms"),
                               choices = NULL,
                               selected = NULL)
          ),


          shinyWidgets::dropMenu(
            tag = shinyWidgets::actionBttn(inputId = "add_input",
                                           label = "Algorithm Input (optional)",
                                           style = "minimal",
                                           color = "success",
                                           size  = "sm"),

            # p -------------------------------------------------------------->>
            shinyWidgets::prettyToggle(inputId = ns("km_reproducible"),
                                       label_on = "Reproducible",
                                       label_off = "Random",
                                       value = TRUE,
                                       thick = TRUE,
                                       shape = "square",
                                       animation = "pulse") |>
              shinyjs::hidden(),


            shiny::numericInput(inputId = ns("n_start"),
                                label = "Number of random set to use.",
                                value = 1, min = 1, max = 10, step = 1) |>
              shinyjs::hidden(),

            shiny::numericInput(inputId = ns("clara_sample"), # on the server side
                                label = "Number of samples to use from the dataset.",
                                value = 0) |>
              shinyjs::hidden(),


            # h -------------------------------------------------------------->>
            shinyWidgets::pickerInput(inputId = ns("hc_method"),
                                      label = "Type of agglomeration method.",
                                      choices = NULL) |>
              shinyjs::hidden(),

            shinyWidgets::prettyRadioButtons(inputId = ns("ad_metric"),
                                             label = "Metric for calculating dissimilarities.",
                                             choices = c("Euclidean" = "euclidean",
                                                         "Manhattan" = "manhattan"),
                                             selected = "euclidean",
                                             shape = "square",
                                             animation = "pulse",
                                             bigger = TRUE) |>
              shinyjs::hidden()
          )
        )
      ),
      shiny::column(
        width = 9,

        shinyWidgets::panel(
          class = "panel-color",

          shiny::uiOutput(outputId = ns("algorithm_summary"))
        )
      )
    ),

    shiny::tags$br(),

    shiny::fluidRow(
      shiny::column(
        width = 3,

        shiny::tags$div(
          class = c("boxIn", "inputpad"),

          shinyjs::hidden(
            shiny::tags$h5(id = ns("su_header"), "Check data suitability"),
            shiny::tags$p(id = ns("su_exp"), paste("This is an optional check, Running this",
                                                   "can take a lot of time for larger datasets.")),
            shinyWidgets::actionBttn(inputId = ns("run_data_suitability"),
                                     label = "Yes",
                                     style = "stretch",
                                     color = "warning")
          ),

          shiny::tags$br(),
          shiny::tags$br(),

          shinyWidgets::prettyRadioButtons(inputId = ns("user_n_cluster_type"),
                                           label = tags$h5("Number of cluster"),
                                           choiceNames = c("Get optimal clusters",
                                                           "Assign manually"),
                                           choiceValues = c("get_optimal", "assign_manually"),
                                           selected = character(0),
                                           shape = "square",
                                           animation = "pulse",
                                           bigger = TRUE),

          shinyWidgets::chooseSliderSkin("Square"),
          shinyjs::hidden(
            sliderInput(inputId = ns("sli_number_centers"),
                        label = "",
                        min = 0, max = 10, value = 0, step = 1)
          ),

          shiny::tags$br(),
          shiny::tags$br(),

          shinyWidgets::actionBttn(inputId = ns("run_cluster_computation"),
                                   label = "Run",
                                   style = "material-flat",
                                   color = "success"),

          shiny::tags$br(),
          shiny::tags$br(),

          shinyWidgets::actionBttn(inputId = ns("go_to_cluster_summary"),
                                   label = "Cluster Summary",
                                   style = "bordered",
                                   color = "success")  |>
            shinyjs::hidden()
        )
      ),

      shiny::column(
        width = 9,

        shinyWidgets::panel(
          class = "panel-color",
          shiny::verbatimTextOutput(outputId = ns("print_data_sutability")) |>
            ui_spinner(),

          shiny::uiOutput(outputId = ns("print_optimal_centers")) |>
            ui_spinner()
        ),

        shiny::tags$br(),

        shinyWidgets::panel(
          class = "panel-color",
          reactable::reactableOutput(outputId = ns("cluster_table")) |>
            ui_spinner()
        ),

        shiny::tags$br(),

        shiny::tags$div(
          class = "center-div",

          shiny::downloadButton(outputId = ns("download_cluster"),
                                label = "Download Data .csv") |>
            shinyjs::hidden()
        )
      )
    )
  )
}




#' run_cluster_analysis Server Functions
#'
#' @param id
#' @param r_use_data_list a list containing the original data and the restructured data.
#' @param parent_session
#'
#' @noRd
mod_run_cluster_analysis_server <- function(id, r_use_data_list, parent_session) {
  stopifnot(shiny::is.reactive(r_use_data_list))

  shiny::moduleServer(
    id = id,

    module = function(input, output, session) {

    ns <- session$ns

    r_orig_data <- shiny::reactive({r_use_data_list()$original_table})
    r_engr_data <- shiny::reactive({r_use_data_list()$engineered_table})

    # Update additional cluster algorithm choices ---------------------------|
    algo_choice <- shiny::reactive({
      list(
        Partition = c("K-partition clustering (kmeans)",
                      "Partitioning Around Medoids (pam)",
                      "Clustering Large Applications"),
        Hierarchical = c("Hierarchical clustering",
                         "Agglomerative Hierarchical Clustering (agnes)",
                         "Divisive Hierarchical Clustering")
      )
    })

    shiny::observe({
      shiny::req(input$algorithm_choice)

      additional_algorithm_choice <- switch(
        input$algorithm_choice,
        `K-partition clustering (kmeans)` = algo_choice()$Partition,
        `Hierarchical clustering` = algo_choice()$Hierarchical
      )

      shiny::updateSelectInput(inputId = "more_algorithm_choice",
                               choices = additional_algorithm_choice,
                               selected = input$algorithm_choice)
    }) |>
      shiny::bindEvent(input$algorithm_choice,
                       label = "more_algorith_choice_observe")

    # Algorithm summary ----------------------------|
    output$algorithm_summary <- shiny::renderUI({
      shiny::req(input$more_algorithm_choice)

      algo_content <- algorithm_summary(input$more_algorithm_choice)

      shiny::tagList(
        algorithm_description(
          title = algo_content$title,
          description = algo_content$content
        )
      )
    })

    # additional input options ------------------------------------------------|
    shiny::observe({
      if (!is.null(input$algorithm_choice)) {

        if (input$algorithm_choice == "K-partition clustering (kmeans)") {

          shinyjs::show(id = "km_reproducible", anim = TRUE)

          if (input$more_algorithm_choice == "K-partition clustering (kmeans)") {
            shinyjs::show(id = "n_start", anim = TRUE)
          } else {
            shinyjs::hide(id = "n_start", anim = TRUE)
          }

          if (input$more_algorithm_choice == "Clustering Large Applications") {
            shinyjs::show(id = "clara_sample", anim = TRUE)
          } else {
            shinyjs::hide(id = "clara_sample", anim = TRUE)
          }

        } else {
          shinyjs::hide(id = "km_reproducible", anim = TRUE)
          shinyjs::hide(id = "n_start", anim = TRUE)
          shinyjs::hide(id = "clara_sample", anim = TRUE)
        }


        if (input$more_algorithm_choice == "Hierarchical clustering" ||
            input$more_algorithm_choice == "Agglomerative Hierarchical Clustering (agnes)") {
          shinyjs::show(id = "hc_method", anim = TRUE)
        } else {
          shinyjs::hide(id = "hc_method", anim = TRUE)
        }

        hcd <- c("Agglomerative Hierarchical Clustering (agnes)",
                 "Divisive Hierarchical Clustering")

        if (input$more_algorithm_choice %in% hcd) {
          shinyjs::show(id = "ad_metric", anim = TRUE)
        } else {
          shinyjs::hide(id = "ad_metric", anim = TRUE)
        }
      }
    })


    # Data suitability --------------------------------------------------------|
    data_points <- shiny::reactive({
      shiny::req(r_engr_data())

      dim(r_engr_data())[1] * dim(r_engr_data())[2]
    })

    shiny::observe({
      shiny::req(data_points())
      if (data_points() <= 2500) {
        shinyjs::show(id = "su_header")
        shinyjs::show(id = "su_exp")
        shinyjs::show(id = "run_data_suitability")

      } else {
        shinyjs::hide(id = "su_header")
        shinyjs::hide(id = "su_exp")
        shinyjs::hide(id = "run_data_suitability")
      }
    })

    shiny::observe({
      shiny::req(data_points())
      if (data_points() >= 2000) {
        shinyWidgets::ask_confirmation(inputId = "user_confirmation",
                                       type = "warning",
                                       title = "Comfirm",
                                       text = paste("Too much data points in the dataset",
                                                    "which can take alot of time.\n",
                                                    "Do you still want to run this check ?"))
      }
    }) |>
      shiny::bindEvent(input$run_data_suitability,
                label = "user_confirmation_observe")

    is_suitable <- shiny::reactive({
      if (input$run_data_suitability > 0 &&
          (isTRUE(input$user_confirmation) || data_points() <= 2500)) {
        data_suitability(df = r_engr_data(), scale = FALSE)
      }
    }) |>
      shiny::bindEvent(input$run_data_suitability,
                       label = "user_confirmation_observe")

    output$print_data_sutability <- shiny::renderPrint({ is_suitable() })
    # NOTE: you should be able to cancel this operation half way.


    # Clusters ----------------------------------------------------------------|
    shiny::observe({
      if (input$more_algorithm_choice == "" && !is.null(input$user_n_cluster_type)) {
        shinyWidgets::show_alert(title = "Something went wrong!!",
                                 text = "An algorithm must be selected",
                                 type = "error")
      }
    })


    optimal_cal_centers <- shiny::reactive({
      shiny::req(r_engr_data(), algo_choice())

      if (!is.null(input$user_n_cluster_type)) {
        if (input$more_algorithm_choice != "" &&
            input$user_n_cluster_type == "get_optimal") {

          kmeans_algs <- algo_choice()$Partition
          hclust_algs <- algo_choice()$Hierarchical

          if (input$more_algorithm_choice %in% kmeans_algs) {
            search_method <- "kmeans"

          } else if (input$more_algorithm_choice %in% hclust_algs) {
            search_method <- "complete"               # ++++++++++++++++++++++
          }

          optimal_center(df = r_engr_data(),
                         search_method = search_method,
                         max_n = 8,
                         type  = "n_cluster")
        }
      }
    })


    output$print_optimal_centers <- shiny::renderPrint({
      shiny::req(optimal_cal_centers())

      if (input$run_data_suitability) {
        shiny::tagList(
          shiny::br(),

          suggested_optimal_cluster_card(optimal_cal_centers())
        )

      } else {
        shiny::tagList(
          suggested_optimal_cluster_card(optimal_cal_centers())
        )
      }
    })


    shiny::observe({
      if (!is.null(input$user_n_cluster_type)) {
        if (input$user_n_cluster_type == "assign_manually") {
          shinyjs::show(id = "sli_number_centers", anim = TRUE)
        } else {
          shinyjs::hide(id = "sli_number_centers", anim = TRUE)
        }
      }
    })

    selected_cluster <- shiny::reactive({
      if (!is.null(input$user_n_cluster_type)) {
        if (input$user_n_cluster_type == "get_optimal") {
          optimal_cal_centers()

        } else if (input$user_n_cluster_type == "assign_manually") {
          input$sli_number_centers
        }
      }
    }) |>
      shiny::bindEvent(input$user_n_cluster_type)

    # Input Error -------------------------------------------------------------|
    shiny::observe({
      if (input$more_algorithm_choice == "" && is.null(input$user_n_cluster_type)) {
        shinyWidgets::show_alert(title = "Something went wrong!!",
                                 text = "An algorithm and either get optimal clusters
                                            or Assign manually must be selected",
                                 type = "error")

      } else if (input$more_algorithm_choice != "" && is.null(input$user_n_cluster_type)) {
        shinyWidgets::show_alert(title = "Something went wrong!!",
                                 text = "Get optimal clusters or Assign manually must be selected",
                                 type = "error")

      } else if (input$more_algorithm_choice == "" && !is.null(input$user_n_cluster_type)) {
        shinyWidgets::show_alert(title = "Something went wrong!!",
                                 text = "An algorithm must be selected",
                                 type = "error")
      }
    }) |>
      shiny::bindEvent(input$run_cluster_computation)

    # clara n samples ---------------------------------------------------------|
    shiny::observe({

      shiny::updateNumericInput(session = session,
                                inputId = "clara_sample",
                                value = nrow(r_orig_data())*0.1,
                                min = 5,
                                max = nrow(r_orig_data())*0.2)
    })


    # h-cluster method --------------------------------------------------------|
    shiny::observe({
      shiny::req(input$more_algorithm_choice)

      if (input$algorithm_choice == "Hierarchical clustering") {
        if (input$more_algorithm_choice == "Hierarchical clustering") {
          hc_m <- c("Single" = "single", "Complete" = "complete", "Average" = "average",
                    "Median" = "median", "ward.D" = "ward.D", "ward.D2" = "ward.D2")

        } else if (input$more_algorithm_choice == "Agglomerative Hierarchical Clustering (agnes)") {
          hc_m <- c("Average"  = "average", "Single" = "single",
                    "Complete" = "complete", "Ward" = "ward",
                    "Weighted" = "weighted", "Flexible" = "flexible")

        } else {
          hc_m <- "Average"
        }

        shinyWidgets::updatePickerInput(session = session,
                                        inputId = "hc_method",
                                        choices = hc_m,
                                        selected = "average")
      }
    })


    # Run cluster analysis ----------------------------------------------------|
    cluster_data <- shiny::reactive({
      shiny::req(r_orig_data(), r_engr_data(), input$more_algorithm_choice, selected_cluster())

      if (!is.null(input$user_n_cluster_type)) {
        if (input$km_reproducible) {
          set.seed(111)
        }

        n_sample <- ifelse(input$clara_sample > 0, input$clara_sample, NULL)

        run_cluster_algo(mdl_df = r_engr_data(),
                         org_df = r_orig_data(),
                         algorithm_type = input$more_algorithm_choice,
                         h_method = input$hc_method,
                         r_metric = input$ad_metric,
                         number_k_centers = selected_cluster(),
                         r_n_starts = input$n_start,
                         r_sample = n_sample)
      }
    }) |>
      shiny::bindEvent(input$run_cluster_computation,
                       label = "run_cluster_computation_reactive")


    output$cluster_table <- reactable::renderReactable({
      clean_reactable_names(x = cluster_data(), include = "cluster") |>
        cluster_segment_reactable(include = "cluster", page_size = 10)
    })


    # Download cluster data -------------------------------------------------|
    shiny::observe({
      shinyjs::show(id = "download_cluster")
    }) |>
      shiny::bindEvent(cluster_data(), label = "show_cdownloadbutton_observe")


    output$download_cluster <- shiny::downloadHandler(
      filename = function() {
        paste("cluster-data-", Sys.Date(), ".csv", sep="")
      },
      content = function(file) {
        vroom::vroom_write(x = cluster_data(), file = file, delim = ",")
      }
    )

    # Show go to `go to cluster summary` panel ------------------------------|
    shiny::observe({
      shiny::req(cluster_data())

      shinyjs::show(id = "go_to_cluster_summary")
    })


    # Switch to cluster summary tab -----------------------------------------|
    shiny::observe({
      shiny::updateNavbarPage(session  = parent_session,
                              inputId  = "navbar_container",
                              selected = "tab_cluster_summary")
    }) |>
      shiny::bindEvent(input$go_to_cluster_summary,
                       label = "go_to_cluster_summary_observe")

    # Return data frame -----------------------------------------------------|
    return(cluster_data)
  }
 )
}

