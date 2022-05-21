server <- function(input, output, session) {
  r_use_data <- data_choice_server(id = "data_choice",
                                   parent_session = session)
  
  list_data <- select_vars_server(id = "select_cluster_variables",
                                  r_use_data = r_use_data,
                                  parent_session = session)
  
  c_data <- run_cluster_analysis_server(id = "run_cluster_analysis",
                                        r_use_data_list = list_data,
                                        parent_session  = session)
  
  cluster_summary_server(id = "cluster_analysis_summary",
                         clust_data = c_data,
                         parent_session = session)
  
  segment_df <- assign_segment_server(id = "assign_segments",
                                      clust_data = c_data,
                                      parent_session = session)
  
  segment_summary_server(id = "summarise_segments",
                         seg_data = segment_df)
  
}
