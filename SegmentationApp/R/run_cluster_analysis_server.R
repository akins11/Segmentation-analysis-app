run_cluster_analysis_server <- function(id, r_use_data_list, parent_session) {
 
  stopifnot(is.reactive(r_use_data_list))

  moduleServer(
    id = id,
    
    module = function(input, output, session) {
      r_orig_data <- reactive({r_use_data_list()$original_table})
      r_engr_data <- reactive({r_use_data_list()$engineered_table})
      
      # Update additional cluster algorithm choices ---------------------------|
      algo_choice <- reactive({
        list(
          Partition = c("K-partition clustering (kmeans)",
                        "Partitioning Around Medoids (pam)",
                        "Clustering Large Applications"),
          Hierarchical = c("Hierarchical clustering",
                           "Agglomerative Hierarchical Clustering (agnes)",
                           "Divisive Hierarchical Clustering")
        )
      })
      
      observe({
        req(input$algorithm_choice)
        
        additional_algorithm_choice <- switch(
          input$algorithm_choice,
          `K-partition clustering (kmeans)` = algo_choice()$Partition,
          `Hierarchical clustering` = algo_choice()$Hierarchical
        )
        
        updateSelectInput(inputId = "more_algorithm_choice",
                          choices = additional_algorithm_choice,
                          selected = input$algorithm_choice)
      }) |>
        bindEvent(input$algorithm_choice,
                  label = "more_algorith_choice_observe")
      
      # Algorithm summary ----------------------------|
      output$algorithm_summary <- renderUI({
        req(input$more_algorithm_choice)
        algorithm_summary(input$more_algorithm_choice)
      })
      
      
      # Data suitability ------------------------------------------------------|
      data_points <- reactive({
        req(r_engr_data())
        
        dim(r_engr_data())[1] * dim(r_engr_data())[2]
      })
      
      observe({
        req(data_points())
        if (data_points() <= 2500) {
          shinyjs::show(id = "su_header")
          shinyjs::show(id = "su_exp")
          shinyjs::show(id = "run_data_suitability")
        }
      })
      
      observe({
        req(data_points())
        if (data_points() >= 2000) {
          ask_confirmation(inputId = "user_confirmation",
                            type = "warning",
                            title = "Comfirm",
                            text = paste("Too much data points in the dataset",
                                         "which can take alot of time.\n",
                                         "Do you still want to run this check ?"))
        } 
      }) |>
        bindEvent(input$run_data_suitability,
                  label = "user_confirmation_observe")
      
      is_suitable <- reactive({
        if (input$run_data_suitability > 0 && 
            (isTRUE(input$user_confirmation) || data_points() <= 2500)) {
          data_suitability(df = r_engr_data(),
                           scale = FALSE)
        }
      }) |>
        bindEvent(input$run_data_suitability,
                  label = "user_confirmation_observe")
      
      output$print_data_sutability <- renderPrint({ is_suitable() })
      # NOTE: you should be able to cancel this operation half way.
      
      
      # Optimal cluster -------------------------------------------------------|
      observe({
        if (input$more_algorithm_choice == "" && input$get_optimal_center > 0) {
          show_alert(title = "Something went wrong!!",
                     text = "An algorithm must be selected",
                     type = "error")
        }
      })
  
      optimal_cal_centers <- reactive({
        req(r_engr_data())
        if (input$more_algorithm_choice != "") {
          kmeans_algs <- algo_choice()$Partition
          hclust_algs <- algo_choice()$Hierarchical

          if (input$more_algorithm_choice %in% kmeans_algs) {
            search_method <- "kmeans"
          } else if (input$more_algorithm_choice %in% hclust_algs) {
            search_method <- "complete"
          }

          optimal_center(df = r_engr_data(),
                         search_method = search_method,
                         max_n = 8,
                         type  = "n_cluster")
        }
      }) |>
        bindEvent(input$get_optimal_center,
                  label = "get_optimal_center_reactive")
      
      output$print_optimal_centers <- renderPrint({
        req(optimal_cal_centers())
        paste("The Optimal number of clusters is", optimal_cal_centers())
      })
      
      observe({
        if (input$get_optimal_center > 0 && input$more_algorithm_choice != "") {
          shinyjs::hide(id = "nc_header")
          shinyjs::hide(id = "nc_exp")
          shinyjs::hide(id = "sli_number_centers")
        }
      }) |>
        bindEvent(input$get_optimal_center)
      
      
      # center assignment 
      optimal_centers <- reactive({
        
        if (input$get_optimal_center > 0 && input$sli_number_centers == 0) {
          optimal_cal_centers()
        } else if (input$get_optimal_center >= 0 && input$sli_number_centers > 0) {
          input$sli_number_centers
        }
      }) |>
        bindEvent(input$get_optimal_center, input$sli_number_centers)
      
      
      # Run cluster analysis --------------------------------------------------|
      observe({
        if (input$more_algorithm_choice == "" &&
            input$get_optimal_center == 0 && input$sli_number_centers == 0) {
          
          show_alert(title = "Something went wrong!!",
                     text = "An algorithm and either `Get Optimal Number Of clusters`
                              or `Assign Number of Clusters` must be selected",
                     type = "error")
          
        } else if (input$more_algorithm_choice != "" &&
                   input$get_optimal_center == 0 && input$sli_number_centers == 0) {
          show_alert(title = "Something went wrong!!",
                     text = "`Get Optimal Number Of clusters` or `Assign Number 
                              of Clusters` must be selected",
                     type = "error")
        }
        else if (input$more_algorithm_choice == "" &&
                   input$sli_number_centers > 0) {
          show_alert(title = "Something went wrong!!",
                     text = "An algorithm must be selected",
                     type = "error")
        }
      }) |>
        bindEvent(input$run_cluster_computation)
      
      
      cluster_data <- reactive({
        req(r_orig_data(), r_engr_data(), 
            input$more_algorithm_choice, optimal_centers())
        
        run_cluster_algo(mdl_df = r_engr_data(),
                         org_df = r_orig_data(),
                         algorithm_type = input$more_algorithm_choice,
                         h_method = "average",
                         number_k_centers = optimal_centers())
      }) |>
        bindEvent(input$run_cluster_computation,
                  label = "run_cluster_computation_reactive")
      
      
      output$cluster_table <- renderReactable({
        clean_reactable_names(x = cluster_data(), include = "cluster") |>
          cluster_segment_reactable(include = "cluster", page_size = 10)
      })
      
      
      # Download cluster data -------------------------------------------------|
      observe({
        shinyjs::show(id = "download_cluster")
      }) |>
        bindEvent(cluster_data(), label = "show_cdownloadbutton_observe")
      
      
      output$download_cluster <- downloadHandler(
        filename = function() {
          paste("cluster-data-", Sys.Date(), ".csv", sep="")
        },
        content = function(file) {
          vroom::vroom_write(x = cluster_data(), file = file, delim = ",")
        }
      )
      
      # Show go to `go to cluster summary` panel ------------------------------|
      observe({
        req(cluster_data())
        
        shinyjs::show(id = "go_to_cluster_summary")
      })
      
      
      # Switch to cluster summary tab -----------------------------------------|
      observe({
        updateNavbarPage(session  = parent_session,
                         inputId  = "navbar_container",
                         selected = "tab_cluster_summary")
      }) |>
        bindEvent(input$go_to_cluster_summary,
                  label = "go_to_cluster_summary_observe")
      
      # Return data frame -----------------------------------------------------|
      return(cluster_data)
    }
  )
}





