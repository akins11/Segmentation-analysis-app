#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
#'
app_ui <- function(request) {
  shiny::tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    bslib::page_navbar(
      title = "Segmentation Analysis",
      id = "navbar_container",

      bg = "#FCFCFC",

      theme = bslib::bs_theme(version = 5,
                              bootswatch = "zephyr",
                              primary = "#424242"),

      windowTitle = "Segmentation analysis",

      header = tags$head(
        tags$link(rel  = "stylesheet", type = "text/css", href = "styles.css"),

        shinyjs::useShinyjs(),

        shiny::tags$script(src = "last_click.js")
      ),

      footer = shiny::tags$div(tags$br(), shiny::tags$br()),


      bslib::nav_spacer(),

      shiny::tabPanel(
        title = "Data Choice",
        value = "tab_data_choice",

        mod_load_data_ui(id = "data_choice")
      ),

      shiny::tabPanel(
        title = "Variable Selection",
        value = "tab_var_selection",

        mod_select_variables_ui(id = "select_cluster_variables")
      ),


      shiny::navbarMenu(
        title = "Cluster Analysis",

        tabPanel(
          title = "Run Analysis",
          value = "tab_run_cluster",

          mod_run_cluster_analysis_ui(id = "run_cluster_analysis")
        ),

        shiny::tabPanel(
          title = "Analysis Summary",
          value = "tab_cluster_summary",

          mod_cluster_summary_ui(id = "cluster_analysis_summary")
        )
      ),


      shiny::navbarMenu(
        title = "Segment Assignment",

        shiny::tabPanel(
          title = "Assign Segment",
          value = "tab_assign_segment",

          mod_assign_segments_ui(id = "assign_segments")
        ),

        shiny::tabPanel(
          title = "Segmentation Summary",
          value = "tab_segment_summary",

          mod_segmentaion_summary_ui(id = "summarise_segments")
        )
      ),

      bslib::nav_spacer(),

      shiny::tabPanel(
        title = "About",
        value = "tab_about",

        markdown(
          "
      ### **Segmentation Analysis app** `version 0.0.15`

      This application can be used for performing market segmentation analysis
      with the use cluster analysis, it consist of dividing a market into groups
      of similar characteristics like age, income, personality, purchasing behavior,
      general behavior etc. you can read more on market segmentation on [Wikipedia](
      https://en.m.wikipedia.org/wiki/Market_segmentation) and also
      [cluster analysis](https://en.m.wikipedia.org/wiki/Cluster_analysis)

      The steps involved in running this analysis are::

      1. Uploading a clean data set which have all columns as variables and
         all rows as observations, rows with missing values will be droped.

      2. Select numerical (more preferred) and character variables, which will
         be used for creating clusters within the data. You don't need to select
         all the available variables in the data. Furthermore few number of variables
         selected will result in less time for the algorithm to compute the analysis.

      3. Choosing a cluster algorithm and also the number of centers to
         group the data into. center selection can be done by running an analysis
         which determine the optimal number of centers in the data or setting a
         preferred number of centers manually. After all important inputs have been
         supplied the next operation will be to run a cluster analysis.

      4. Creating cluster summaries using the avaliable variables in the dataset,
         although this is optional but it will help in creating a visual image of
         how the segments should turn out.

      5. Assign preferred segments to the data manually or use an aggregate
         summary. Results of the segments created can be downloaded and summaries
         of each segments can be viewed in the segmentation summary panel.


      **Contact**: [akinwandeayomide24@gmail.com](mailto:akinwandeayomide24@gmail.com)
      "
        )
      ),

      bslib::nav_item(
        shiny::tags$a("Website", href = "https://akins11.github.io/Portfolio/")
        #fontawesome::fa_i("fas fa-info"),
      ),
    )
  )
}



#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(ico = "dibcon"),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "segmentationAnalysisApp"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
