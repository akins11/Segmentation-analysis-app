#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic
  r_use_data <- mod_load_data_server(id = "data_choice",
                                    parent_session = session)

  list_data <- mod_select_variables_server(id = "select_cluster_variables",
                                           r_use_data = r_use_data,
                                           parent_session = session)

  c_data <- mod_run_cluster_analysis_server(id = "run_cluster_analysis",
                                            r_use_data_list = list_data,
                                            parent_session  = session)

  mod_cluster_summary_server(id = "cluster_analysis_summary",
                             clust_data = c_data,
                             parent_session = session)

  segment_df <- mod_assign_segments_server(id = "assign_segments",
                                           clust_data = c_data,
                                           parent_session = session)

  mod_segmentaion_summary_server(id = "summarise_segments",
                                 seg_data = segment_df)
}
