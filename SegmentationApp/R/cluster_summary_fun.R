#' Variable Data Type
#'
#' @param df data.frame
#' @param type character: The type of variable either 'character' or 'numeric'.
#' @param remove_var character: The variable not to include.
#'
#' @return A vector of column names with the input type.
#' @export
#'
#' @examples
get_var_type_names <- function(df, type, remove_var) {
  
  type <- match.arg(type, c("character", "numeric"))
  
  if (type == "character") {
    is <- sapply(df, \(.x) is.character(.x) | is.factor(.x) | is.ordered(.x))
  } else if (type == "numeric") {
    is <- sapply(df, \(.x) is.numeric(.x) | is.double(.x) | is.integer(.x))
  }
  
  output <- names(df[is])
  
  if (missing(remove_var)) {
    output
  } else {
    if (length(remove_var) == 1) {
      output[which(output != remove_var)]
    } else {
      output[which(!output %in% remove_var)]
    }
  }
}




#' Check Variable Data Type
#'
#' @param df data.frame
#' @param with_type character: The type of variable. either character or numeric.
#'
#' @return TRUE if the data contains the input type else FALSE.
#' @export
#'
#' @examples
is_with_type <- function(df, with_type) {
  
  with_type <- match.arg(with_type, c("character", "numeric"))
  
  if (with_type == "character") {
    is <- sapply(df, \(.x) is.character(.x) | is.factor(.x) | is.ordered(.x)) 
    
  } else if (with_type == "numeric") {
    is <- sapply(df, \(.x) is.numeric(.x) | is.double(.x) | is.integer(.x)) 
  }
  
  any(as.vector(is) == TRUE)
}




#' Number Of Cluster 
#'
#' @description Create a summary count of clusters using a table or a plot.
#'
#' @param df data.frame: A df with a variable '.cluster' in it.
#' @param output_type character: The type of output to return, either a 'table'
#' or a 'plot'
#'
#' @return if argument output_type == 'table' a data.frame else if it is 'plot'
#' a ggplot object.
#' @export
#'
#' @examples
count_clusters <- function(df, output_type = "table") {
  
  output_type <- match.arg(output_type, c("table", "plot"))
  
  f_tbl <- dplyr::count(df, .cluster, sort = TRUE, name = "count") |>
    dplyr::mutate(proportion = round(proportions(count)*100, 2))

  if (output_type == "table") {
    f_tbl
  } else if (output_type == "plot"){
    f_tbl |> 
      ggplot2::ggplot(ggplot2::aes(x = forcats::fct_reorder(factor(.cluster), count, .desc = TRUE), 
                                   y = count)) +
      ggplot2::geom_col(fill = plt_clr$bars) +
      ggplot2::geom_text(ggplot2::aes(label = paste0(proportion,"%"),
                                      vjust = -0.2),
                         position = ggplot2::position_dodge(.9) ) +
      ggplot2::labs(x = "Clusters", 
                    y = "Count", 
                    title = "Number Of Record In Each Cluster") +
      ggplot2::theme_minimal()
  }
}



#' Number of Catagories in Each Cluster.
#'
#' @param df data.frame: A data frame with a variable '.cluster' in it.
#' @param chr_var character: A variable from the data.
#' @param n_obs numeric: Useful when the number of categories are more than 10.
#' @param output_type character: The type of output to return either a 'plot'
#' or a table.
#'
#' @return if argument output_type == 'table' a data.frame else if it is 'plot'
#' a ggplot object.
#' @export
#'
#' @examples
chr_count_cluster <- function(df, chr_var, n_obs = 10, output_type = "table") {
  
  f_tbl <- dplyr::count(df, .data[[chr_var]], .cluster, name = "count") |>
    dplyr::arrange(.cluster, desc(count))

  if (output_type == "table") {
    f_tbl
    
  } else if (output_type == "plot") {
    unique_cat <- length(unique(df[[chr_var]]))
    chr_lb <- plot_labels(chr_var)
    
    if (unique_cat < 10) {
      f_plt <- f_tbl |>
        ggplot2::ggplot(ggplot2::aes(x = forcats::fct_reorder2(.data[[chr_var]], 
                                                               .cluster, count),
                                     y = count))
      if (unique_cat > 6) {
        f_plt <- f_plt +
          ggplot2::geom_col(ggplot2::aes(fill = .cluster), show.legend = FALSE) +
          ggplot2::coord_flip() +
          ggplot2::theme_minimal()
      } else {
        f_plt <- f_plt +
          ggplot2::geom_col(ggplot2::aes(fill = .cluster), show.legend = FALSE) +
          ggplot2::theme_minimal() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 18,
                                                             hjust = 1, vjust =1.7))
      }
      f_plt +
        ggplot2::facet_wrap(ggplot2::vars(.cluster), scales = "free") +
        ggplot2::scale_fill_manual(values = rev(plt_clr$dash)) +
        ggplot2::labs(x = NULL, y = NULL, 
                      title = paste("Count By", chr_lb, "In Each Cluster"))
      
    } else {
      uq_count <- dplyr::count(df, .data[[chr_var]]) |> 
        dplyr::distinct(n) |> 
        dplyr::pull()
      
      if (length(uq_count) > 1) {
        df$char_var <- forcats::fct_lump(df[[chr_var]], 
                                         n = n_obs, 
                                         other_level = paste("other", chr_var))
      } else {
        uq <- unique(df[[chr_var]])
        df$char_var <- forcats::fct_other(df[[chr_var]], 
                                          keep = sample(uq, n_obs), 
                                          other_level = paste("other", chr_var))
      }
      dplyr::count(df, char_var, .cluster) |>
        ggplot2::ggplot(ggplot2::aes(x = n, y = forcats::fct_rev(char_var), fill = .cluster)) +
        ggplot2::geom_col(position = ggplot2::position_fill()) +
        ggplot2::scale_x_continuous(labels = scales::percent_format()) +
        ggplot2::scale_fill_manual(values = rev(plt_clr$dash)) +
        ggplot2::labs(y = chr_lb, 
                      x = NULL, 
                      title = paste("Proportion Of Count By", chr_lb, "In Each Cluster")) +
        ggplot2::theme_minimal()
    }
  }
}




#' Cluster Summary With Numerical Variables
#'
#' @param df data.frame
#' @param var numeric: a variable from the data to summarise.
#' @param col_names_to_title logical: Make the column names in a title format.
#'
#' @return A summarized data.frame/tibble.
#' @export
#'
#' @examples
numeric_stat_summary <- function(df, var, col_names_to_title = FALSE) {
  
  f_tbl <- dplyr::summarise(df,
                            count       = dplyr::n(),
                            minimum     = min(.data[[var]], na.rm = TRUE),
                            quantile_25 = quantile(.data[[var]], 0.25),
                            mean        = mean(.data[[var]], na.rm = TRUE),
                            median      = median(.data[[var]], na.rm = TRUE),
                            quantile_75 = quantile(.data[[var]], 0.75),
                            maximum     = max(.data[[var]], na.rm = TRUE),
                            sum         = sum(.data[[var]], na.rm = TRUE))
  
  if (isTRUE(col_names_to_title)) {
    dplyr::rename_with(f_tbl, stringr::str_to_title)
  } else {
    f_tbl
  }
}



#' Extract Summary
#'
#' @description Extract a aggregate column from a list of data frame.
#'
#' @param list_df list: A list of Data.frames with a variable '.cluster' in it.
#' @param get_stat character: An aggregate variable from the list data.frame
#' @param pivot logical: TRUE to reshape the data.frame to a long format.
#'
#' @return data.frame/tibble.
#' @export
#'
#' @examples
pluck_stat_summary <- function(list_df, get_stat, pivot = TRUE) {
  
  f_tbl <- purrr::map(list_df, purrr::pluck, get_stat) |>
    tibble::as_tibble() |>
    dplyr::mutate(.cluster = unique(list_df[[1]][[".cluster"]]), .before = 1)
  
  if (isTRUE(pivot)) {
    tidyr::pivot_longer(data = f_tbl, 
                        cols = all_of(names(list_df)),
                        names_to  = "variable",
                        values_to = get_stat)
    
  } else {
    f_tbl
  }
}




#' Cleaned Plot Labels
#'
#' @param label character: string to clean.
#' @param label_list list: A list of default labels to replace.
#'
#' @return A cleaned plot label.
#' @export
#'
#' @examples
plot_labels <- function(label, label_list) {
  
  new_label <- stringr::str_replace_all(label, "[:punct:]", " ") |> 
    stringr::str_to_title()
  
  if (!missing(label_list)) {
    if (!label %in% names(label_list)) {
      new_label
    } else {
      label_list[[label]]
    }
  } else {
    new_label
  }
}




#' Aggregate Summary For Each Cluster
#' 
#' @description Return a single or multiple descriptive summary table.
#'
#' @param df data.frame: A df with a variable '.cluster' in it.
#' @param num_variables numeric: A single or multiple variables from the data.
#' @param plot_stat_var character: stat description variable to use in the plot.
#' @param output_type character: The type of output to return, either 'plot'
#' or 'table'
#'
#' @return if argument output_type == 'table' a data.frame else if it is 'plot'
#' a ggplot object.
#' @import plot_labels, pluck_stat_summary, numeric_stat_summary
#' @export
#'
#' @examples
cluster_stat_summary <- function(df, 
                                 num_variables, 
                                 plot_stat_var,
                                 output_type = "plot") {
  #'@description Return a single or multiple descriptive summary table.
  #'@param df data.frame: A df with a variable '.cluster' in it.
  #'@param num_variables numeric: A single or multiple variables from the data.
  #'@param output_type character: The type of output to return, either 'plot' 
  #'or 'table'
  #'@param plot_stat_var character: stat description variable to use in the plot.
  #'@import plot_labels, pluck_stat_summary, numeric_stat_summary
  
  output_type <- match.arg(output_type, c("table", "plot"))
  
  get_summary <- function(var) {
    dplyr::group_by(df, .cluster) |>
      numeric_stat_summary(var)
  }
  
  if (output_type == "plot") {
    plt_l <- plot_labels(plot_stat_var, stat_label)
    var_l <- plot_labels(num_variables)
  }
  
  if (length(num_variables) == 1) {
    f_tbl <- get_summary(num_variables)
    
    if (output_type == "table") {
      f_tbl |>
        dplyr::rename(first_quantile = quantile_25,
                      third_quantile = quantile_75)
      
    } else if (output_type == "plot") {
      f_tbl |>
        ggplot2::ggplot(ggplot2::aes(x = forcats::fct_reorder(.cluster, 
                                                              .data[[plot_stat_var]], 
                                                              .desc = TRUE), 
                                     y = .data[[plot_stat_var]])) +
        ggplot2::geom_col(fill = plt_clr$bars) +
        ggplot2::scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
        ggplot2::labs(x = "Cluster", 
                      y = plt_l, 
                      title = paste(plt_l, var_l, "In Each Cluster")) +
        ggplot2::theme_minimal()
    }
    
  } else {
    list_df <- lapply(num_variables, get_summary)
    names(list_df) <- num_variables
    
    if (output_type == "table") {
      lapply(list_df, function(.x) {
        dplyr::rename(.x,
                      first_quantile = quantile_25,
                      third_quantile = quantile_75)
      })
      
    } else if (output_type == "plot") {
      var_ll <- paste(var_l, collapse = ", ")
      
      pluck_stat_summary(list_df, plot_stat_var, pivot = TRUE) |>
        ggplot2::ggplot(ggplot2::aes(x = .cluster, 
                                     y = .data[[plot_stat_var]])) +
        ggplot2::geom_col(fill = plt_clr$bars) +
        ggplot2::facet_wrap(ggplot2::vars(variable), scales = "free_y", 
                            labeller = ggplot2::labeller(variable = plot_labels)) +
        ggplot2::scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
        ggplot2::labs(x = "Cluster", 
                      y = plt_l, 
                      title = paste(plt_l, var_ll, "In Each Cluster")) +
        ggplot2::theme_minimal()
    }
    
  }
}




#' Numerical Relationship Plot with Clusters.
#'
#' @description Return a cluster relationship plot with two variables.
#'
#' @param df data.frame: A df with a variable '.cluster' in it.
#' @param num_varx numeric: x axis variable from the data.
#' @param num_vary numeric: y axis variable from the data.
#'
#' @return ggplot object.
#' @import plot_labels.
#' @export
#'
#' @examples
cluster_relationship_plot <- function(df, num_varx, num_vary) {
  x_len <- length(unique(df[[num_varx]]))
  y_len <- length(unique(df[[num_vary]]))
  
  x_lb <- plot_labels(num_varx)
  y_lb <- plot_labels(num_vary)
  
  f_plt <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[num_varx]], 
                                            y = .data[[num_vary]],
                                            color = .cluster)) 
  if (any(c(x_len, y_len) < 10)) {
    f_plt <- f_plt + ggplot2::geom_jitter()
  } else  {
    f_plt <- f_plt + ggplot2::geom_point()
  }
  
  f_plt +  
    ggplot2::labs(x = x_lb,
                  y = y_lb,
                  title = paste("Cluster Relationship Between", 
                                x_lb, "&", y_lb)) +
    ggplot2::scale_color_manual(values = rev(plt_clr$dash)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "top", 
                   legend.margin   = ggplot2::margin(t = -7, b = -8)) +
    ggplot2::scale_y_continuous(labels = scales::comma_format(1)) +
    ggplot2::scale_x_continuous(labels = scales::comma_format(1)) +
    ggplot2::guides(color = ggplot2::guide_legend(title = NULL,
                                                  label.position = "top",
                                                  label.vjust  = -2,
                                                  override.aes = list(size = 2)))
}