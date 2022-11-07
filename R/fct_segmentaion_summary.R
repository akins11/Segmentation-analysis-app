#' One Character Variable Segment Summary
#'
#' @description create a count summary of the number of records in each segment
#' and character variable.
#'
#' @param df data.frame
#' @param chr_var character: A character variable in the data.
#' @param output_type character: The type of output to return either a 'plot'
#' or 'table'
#'
#' @return if argument output_type == 'table' a data.frame else if it is 'plot'
#' a ggplot object.
#' @export
#'
#' @examples
one_chr_variable <- function(df, chr_var, output_type = "plot") {

  output_type <- match.arg(output_type, c("plot", "table"))

  f_tbl <- df |>
    dplyr::group_by(.segment, .data[[chr_var]]) |>
    dplyr::count(name = "count") |>
    dplyr::arrange(.segment, count) |>
    dplyr::ungroup()

  if (output_type == "table") {
    tidyr::pivot_wider(f_tbl,
                       id_cols = .data[[chr_var]],
                       names_from = .segment,
                       values_from = count, values_fill = 0) |>
      janitor::adorn_totals(where = "both", name = ".Total")

  } else if (output_type == "plot") {
    nb_s <- length(unique(df[[".segment"]]))
    uq_c <- length(unique(df[[chr_var]]))
    y_lb <- plot_labels(chr_var)

    f_tbl |>
      ggplot2::ggplot(ggplot2::aes(x = .segment,
                                   y = .data[[chr_var]],
                                   fill = count)) +
      ggplot2::geom_tile(show.legend = FALSE) +
      ggplot2::geom_vline(xintercept = seq_len(nb_s)-0.5, color = "gray87", size = 1) +
      ggplot2::geom_hline(yintercept = seq_len(uq_c)-0.5, color = "gray87", size = 1) +
      ggplot2::geom_text(ggplot2::aes(label = scales::comma(count, 1)), color = "white", size = 5) +
      ggplot2::scale_x_discrete(expand = c(0, 0)) +
      ggplot2::scale_y_discrete(expand = c(0, 0),
                                label = \(.x) stringr::str_trunc(.x, width = 15, side = "right")) +
      # ggplot2::scale_fill_gradient2(low  =  "gray87",
      #                               mid  = "gray55",
      #                               high = "gray25") +
      ggplot2::scale_fill_gradient2(low  =  "#DEE2E6",
                                    mid  = "#6C757D",
                                    high = "#212529") +
      ggplot2::labs(x = "Segment",
                    y = y_lb,
                    title=paste("Number of Records Of", y_lb, "In Each Segment")) +
      ggplot2::theme_minimal() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(margin = ggplot2::margin(-2, 0, 0, 0),
                                                         hjust  = 1),
                     axis.text.y = ggplot2::element_text(margin = ggplot2::margin(0, -0.5, 0, 0)),
                     panel.grid.major = ggplot2::element_blank(),
                     axis.title.x = ggplot2::element_text(vjust = -1),
                     plot.background = ggplot2::element_rect(fill = plt_plot_BC, color = plt_plot_BC))
  }
}




#' Character variable With The Highest Unique Values.
#'
#' @description Sort 2 variables for the largest category in the data.
#'
#' @param s_df data.frame
#' @param s_vars variable from the data 's_df' to sort.
#'
#' @return sorted vector.
#' @export
#'
#' @examples
sort_vars <- function(s_df, s_vars) {

  sort_list <- purrr::map(s_vars, ~length(unique(s_df[[.x]])))
  names(sort_list) <- s_vars

  max_no <- max(sort_list[[s_vars[1]]], sort_list[[s_vars[2]]])

  output <- ifelse(sort_list[[s_vars[1]]] == max_no,
                   list(c(s_vars[1], s_vars[2])),
                   list(c(s_vars[2], s_vars[1])) )[[1]]
  return(output)
}




#' Lump Large Character Unique Values.
#'
#' @param df data.frame
#' @param chr_var1 character: A variable from the data.frame.
#' @param chr_var2 character: A variable from the data.frame.
#' @param n_max numeric: Maximum number of Unique values to include.
#' @param seed logical: set.seed for samples.
#'
#' @return A lumped character variable.
#' @export
#'
#' @examples
lump_large_chr_count <- function(df, chr_var1, chr_var2, n_max = 10, seed = FALSE) {

  top_n <- function(char_var) {
    dplyr::count(df, .data[[char_var]], sort = TRUE) |>
      head(n = n_max) |>
      dplyr::pull(.data[[char_var]])
  }

  single_filter <- function(char_var) {
    uq_count <- dplyr::count(df, .data[[char_var]]) |>
      dplyr::distinct(n) |>
      dplyr::pull()

    if (length(uq_count) > 1) {
      top_char <- top_n(char_var = char_var)
      dplyr::filter(df, .data[[char_var]] %in% top_char)

    } else {
      unq_var <- unique(df[[char_var]])

      if (isTRUE(seed)) { set.seed(112) }
      df[]
      dplyr::filter(df, .data[[char_var]] %in% sample(unq_var, n_max))
    }
  }

  len_unq1 <- length(unique(df[[chr_var1]]))

  if (missing(chr_var2)) {
    if (len_unq1 > 10) {
      return(single_filter(char_var = chr_var1) )
    } else {
      return(df)
    }
  }
  if (!missing(chr_var2)) {
    len_unq2 <- length(unique(df[[chr_var2]]))

    get_unq_count <- function(char_var) {
      dplyr::count(df, .data[[char_var]]) |>
        dplyr::distinct(n) |>
        dplyr::pull()
    }

    if (len_unq1 > 10 && len_unq2 > 10) {
      uq_count1 <- get_unq_count(chr_var1)
      uq_count2 <- get_unq_count(chr_var2)

      if (length(uq_count1) > 1 && length(uq_count1) > 1) {
        top_char1 <- top_n(char_var = chr_var1)
        top_char2 <- top_n(char_var = chr_var2)

        dplyr::filter(
          .data = df,
          .data[[chr_var1]] %in% top_char1 | .data[[chr_var2]] %in% top_char2
        )

      } else if (length(uq_count1) > 1 && length(uq_count2) == 1) {
        top_char1 <- top_n(char_var = chr_var1)
        dplyr::filter(.data = df, .data[[chr_var1]] %in% top_char1)

      } else if (length(uq_count1) == 1 && length(uq_count2) > 1) {
        top_char2 <- top_n(char_var = chr_var2)
        dplyr::filter(.data = df, .data[[chr_var2]] %in% top_char2)

      } else {
        unq_var1 <- unique(df[[chr_var1]])
        unq_var2 <- unique(df[[chr_var2]])

        if (isTRUE(seed)) { set.seed(112) }
        dplyr::filter(
          .data = df,
          .data[[chr_var1]] %in% sample(unq_var1, n_max) | .data[[chr_var2]] %in% sample(unq_var2, n_max)
        )
      }
    } else if (len_unq1 > 10 && len_unq2 <= 10) {
      return(single_filter(char_var = chr_var1))

    } else if (len_unq1 <= 10 && len_unq2 > 10) {
      return(single_filter(char_var = chr_var2))

    } else {
      df
    }
  }
}




#' Two Character Variable Segment Summary
#'
#' @param df data.frame
#' @param chr_var1 character: A variable from the data.
#' @param chr_var2 character: A variable from the data.
#' @param n_cat numeric: Number of unique values values to include.
#' @param output_type character: The type of output to return either a 'table'
#' 'plot_c' count plot, or 'plot_p' a proportion plot.
#'
#' @return if argument output_type == 'table' a data.frame else a ggplot object.'
#' @import sort_vars, plot_labels
#' @export
#'
#' @examples
two_chr_variable <- function(df,
                             chr_var1,
                             chr_var2,
                             n_cat = 10,
                             output_type = "plot_c") {

  output_type <- match.arg(output_type, c("plot_c", "plot_p", "table"))

  f_tbl <- df |>
    dplyr::count(.segment, .data[[chr_var1]], .data[[chr_var2]], name = "count")

  if (output_type == "table") {
    f_tbl |>
      tidyr::pivot_wider(id_cols = tidyselect::all_of(c(chr_var1, chr_var2)),
                         names_from  = .segment,
                         values_from = count,
                         values_fill = 0) |>
      janitor::adorn_totals(where = "both", name = ".Total")

  } else if (output_type %in% c("plot_c", "plot_p")) {
    plt_tbl <- lump_large_chr_count(f_tbl, chr_var1, chr_var2, n_max = n_cat)

    sort_chr_vars <- sort_vars(df, c(chr_var1, chr_var2))
    var_l <- sort_chr_vars[1]
    var_s <- sort_chr_vars[2]

    len_unique_chr <- length(unique(df[[var_l]]))

    var_llb <- plot_labels(var_l)
    var_slb <- plot_labels(var_s)

    f_plt <- plt_tbl |>
      ggplot2::ggplot(ggplot2::aes(x = .data[[var_l]], y = count, fill = .segment)) +
      ggplot2::labs(x = var_llb, y = NULL, fill = "Segment") +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.key.size = ggplot2::unit(0.4, "cm"),
                     legend.title = ggplot2::element_text(size = 8),
                     axis.title.x = ggplot2::element_text(size = 9, vjust = -1),
                     plot.background = ggplot2::element_rect(fill = plt_plot_BC,
                                                             color = plt_plot_BC))

    if (len_unique_chr > 5) {
      f_plt <- f_plt + ggplot2::coord_flip()
    }

    if (output_type == "plot_c") {
      f_plt +
        ggplot2::geom_col() +
        ggplot2::facet_wrap(ggplot2::vars(.data[[var_s]]), scales = ifelse(len_unique_chr > 5,
                                                                           "free", "free_y")) +
        ggplot2::scale_y_continuous(labels = scales::comma_format()) +
        ggplot2::scale_fill_manual(values = plt_clr$dash) +
        ggplot2::ggtitle(paste("Count of",var_llb,"&",var_slb,"In Each Segment"))

    } else if (output_type == "plot_p") {
      f_plt +
        ggplot2::geom_col(position = ggplot2::position_fill()) +
        ggplot2::facet_wrap(ggplot2::vars(.data[[var_s]]), scales = ifelse(len_unique_chr > 5,
                                                                           "free_y", "fixed")) +
        ggplot2::scale_y_continuous(labels = scales::percent_format()) +
        ggplot2::scale_fill_manual(values = plt_clr$dash) +
        ggplot2::ggtitle(paste("Proportion of Each Segment In",var_llb,"&",var_slb))
    }
  }
}




#' One Numeric Variable Segment Summary
#'
#' @param df data.frame
#' @param num_var numeric: A variable from the data.frame.
#' @param output_type character: The type of out put to return one of 'table',
#' 'plot_at' for an average sum plot and 'plot_mm' for a min max plot.
#'
#' @return if argument output_type == 'table' a data.frame else a ggplot object.
#' @import plot_labels.
#' @export
#'
#' @examples
one_num_variable <- function(df, num_var, output_type = "plot_at") {

  output_type <- match.arg(output_type, c("table", "plot_at", "plot_mm"))

  f_tbl <- df |>
    dplyr::group_by(.segment) |>
    dplyr::summarise(Minimum = min(.data[[num_var]], na.rm = TRUE),
                     Average = mean(.data[[num_var]], na.rm = TRUE),
                     Median  = median(.data[[num_var]], na.rm = TRUE),
                     Maximum = max(.data[[num_var]], na.rm = TRUE),
                     Sum     = sum(.data[[num_var]], na.rm = TRUE))

  if (output_type == "table") {
    f_tbl

  } else if (output_type %in% c("plot_at", "plot_mm")) {
    num_lb <- plot_labels(num_var)
    len_segment <- length(unique(df[[".segment"]]))

    if (output_type == "plot_at") {
      f_tbl |>
        dplyr::select(-c(Minimum, Maximum)) |>
        tidyr::pivot_longer(cols = c(Average,Sum),
                            names_to = "summary", values_to = "value") |>

        ggplot2::ggplot(ggplot2::aes(x = forcats::fct_reorder(.segment, value, .desc = TRUE),
                                     y = value)) +
        ggplot2::geom_col(fill = plt_clr$bars) +
        ggplot2::facet_wrap(ggplot2::vars(summary),
                            scales = "free_y",
                            ncol   = 2,
                            labeller = ggplot2::labeller(summary = \(x) paste(x, num_lb) )) +
        ggplot2::scale_y_continuous(labels = scales::comma_format(1)) +
        ggplot2::labs(x = "Segment", y = NULL) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = ifelse(len_segment > 6, 35, 0),
                                                           hjust = if (len_segment > 6) 1 else NULL),
                       plot.background = ggplot2::element_rect(fill = plt_plot_BC, color = plt_plot_BC))

    } else if (output_type == "plot_mm") {
      f_tbl |>
        ggplot2::ggplot() +
        ggplot2::geom_segment(ggplot2::aes(x = Minimum,  xend = Maximum,
                                           y = .segment, yend = .segment),
                              linetype = "twodash",
                              color    = "gray53") +
        ggplot2::geom_point(ggplot2::aes(x = Minimum, y = .segment), size = 5,
                            shape  = 21,
                            color  = "azure3",
                            fill   = "azure4",
                            stroke = 1.5) +
        ggplot2::geom_point(ggplot2::aes(x = Maximum, y = .segment), size = 8,
                            shape  = 21,
                            color  = "gray70",
                            fill   = "gray26",
                            stroke = 1.5) +
        ggplot2::scale_x_continuous(labels = scales::comma_format(1)) +
        ggplot2::labs(x = NULL, y = "Segments",
                      title = glue::glue("<span style = 'font-size:13pt'>
                                        <span style='color:{'azure4'};'>Minimum</span> &
                                        <span style='color:{'gray26'};'>Maximum</span>
                                        {num_lb} In Each Segment </span>")) +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.title = ggtext::element_markdown(lineheight = 1.1),
                       plot.background = ggplot2::element_rect(fill = plt_plot_BC, color = plt_plot_BC))
    }
  }
}




#' Two Numeric Variable Segment Summary
#'
#' @description Create a summary table or a plot showing the relationship
#' between the variables in each segment.
#'
#' @param df data.frame
#' @param num_varx numeric: Variables from the data.
#' @param num_vary numeric: Variables from the data.
#' @param output_type character: The type of output to return either 'table'
#' 'plot_full' plot with out facets 'plot_facet' with facets.
#'
#' @return if argument output_type == 'table' a data.frame else a ggplot object.
#' @import plot_labels, numeric_stat_summary
#' @export
#'
#' @examples
two_num_variable <- function(df, num_varx, num_vary, output_type = "plot_full") {

  output_type <- match.arg(output_type, c("plot_full", "plot_facet", "table"))

  if (output_type == "table") {
    f_tbl <- lapply(c(num_varx, num_vary), function(.x) {
      dplyr::group_by(df, .segment) |>
        numeric_stat_summary(.x, col_names_to_title = TRUE)
    }) |>
      lapply(function(.x) {
        dplyr::rename(.x,
                      first_quantile = Quantile_25,
                      third_quantile = Quantile_75)
      })
    names(f_tbl) <- c(num_varx, num_vary)
    return(f_tbl)

  } else if (output_type %in% c("plot_full", "plot_facet")) {
    x_len <- length(unique(df[[num_varx]]))
    y_len <- length(unique(df[[num_vary]]))

    x_lb <- plot_labels(num_varx)
    y_lb <- plot_labels(num_vary)

    f_plt <- ggplot2::ggplot(df, ggplot2::aes(x = .data[[num_varx]],
                                              y = .data[[num_vary]],
                                              color = .segment))

    if (any(c(x_len, y_len) < 15)) {
      f_plt_points <- ggplot2::geom_jitter()
    } else  {
      f_plt_points <- ggplot2::geom_point()
    }

    if (output_type == "plot_full") {
      f_plt +
        f_plt_points +
        ggplot2::scale_color_manual(values = plt_clr$dash) +
        ggplot2::scale_x_continuous(labels = scales::comma_format()) +
        ggplot2::scale_y_continuous(labels = scales::comma_format()) +
        ggplot2::theme_minimal() +
        ggplot2::labs(x = x_lb,
                      y = y_lb,
                      title = paste("Relationship Between",
                                    x_lb, "&", y_lb, "In Each Segment")) +
        ggplot2::theme(legend.position = "top",
                       legend.margin   = ggplot2::margin(t = -7, b = -8),
                       plot.background = ggplot2::element_rect(fill = plt_plot_BC,
                                                               color = plt_plot_BC)) +
        ggplot2::guides(color = ggplot2::guide_legend(title = NULL,
                                                      label.position = "top",
                                                      label.vjust  = -2,
                                                      override.aes = list(size = 2)))

    } else if (output_type == "plot_facet") {
      if (any(c(x_len, y_len) < 15)) {
        f_plt <- f_plt +
          ggplot2::geom_jitter(data = ~dplyr::select(., -.segment), color = "grey")
      } else {
        f_plt <- f_plt +
          ggplot2::geom_point(data = ~dplyr::select(., -.segment), color = "grey")
      }

      f_plt +
        f_plt_points +
        ggplot2::facet_wrap(ggplot2::vars(.segment), scale = "fixed") +
        ggplot2::scale_x_continuous(labels = scales::comma_format()) +
        ggplot2::scale_y_continuous(labels = scales::comma_format()) +
        ggplot2::labs(x = x_lb,
                      y = y_lb) +
        ggplot2::scale_color_manual(values = plt_clr$dash) +
        ggplot2::guides(color = "none") +
        ggplot2::theme_minimal() +
        ggplot2::theme(plot.background = ggplot2::element_rect(fill = plt_plot_BC,
                                                               color = plt_plot_BC))
    }
  }
}




#' Find Outlier in a Variable
#'
#' @param df data.frame
#' @param variable numeric: A variable in the data.frame
#' @param type character: The type of outlier to return either 'all', or
#' some of 'strong_lower', 'weak_lower', 'weak_upper', & 'strong_upper'
#'
#' @return A vector corresponding to the supplied type.
#' @export
#'
#' @examples
get_outlier <- function(df, variable, type = "all") {

  var <- df[[variable]]

  strong_lower <- quantile(var, 0.25) - IQR(var, na.rm = TRUE)*3
  weak_lower   <- quantile(var, 0.25) - IQR(var, na.rm = TRUE)*1.5
  weak_upper   <- IQR(var, na.rm = TRUE)*1.5 + quantile(var, 0.75)
  strong_upper <- IQR(var, na.rm = TRUE)*3   + quantile(var, 0.75)

  outlier <- c(strong_lower, weak_lower, weak_upper, strong_upper)
  default_names <- c("strong_lower", "weak_lower", "weak_upper", "strong_upper")
  names(outlier) <- default_names

  if (any(type == "all")) {
    return(outlier)

  } else if (all(type %in% default_names)) {
    as.vector(outlier[type])

  } else {
    stop(paste("Invalid `type` of outlier `type` must be 'all' or",
               "some of", paste(default_names, collapse = ", ")))
  }
}




#' Check For Outlier
#'
#' @param df data.frame
#' @param variable numeric: A variable from the data.
#' @param test character: The type of outlier to test any of 'less_strong_l'
#' 'less_weak_l', 'more_weak_u', 'more_strong_u'
#'
#' @return TRUE if condition to check is true else FALSE
#' @import get_outlier.
#' @export
#'
#' @examples
is_with_outlier <- function(df, variable, test) {

  valid_test <- c("less_strong_l", "less_weak_l", "more_weak_u", "more_strong_u")
  test <- match.arg(test, c(valid_test, "less_more_strong", "less_more_weak"))

  valid_type <- c("strong_lower", "weak_lower", "weak_upper", "strong_upper")
  names(valid_type) <- valid_test
  if (test %in% valid_test) {
    outlier <- get_outlier(df = df, variable = variable, type = valid_type[test])
  }

  if (test == "less_strong_l") {
    if (any(df[[variable]] <= outlier)) TRUE else FALSE

  } else if (test == "less_weak_l") {
    if (any(df[[variable]] <= outlier)) TRUE else FALSE

  } else if (test == "more_weak_u") {
    if (any(df[[variable]] >= outlier)) TRUE else FALSE

  } else if (test == "more_strong_u") {
    if (any(df[[variable]] >= outlier)) TRUE else FALSE

  } else if (test == "less_more_strong") {
    less_strong <- get_outlier(df = df, variable = variable, type = "strong_lower")
    more_strong <- get_outlier(df = df, variable = variable, type = "strong_upper")

    if (any(df[[variable]] <= less_strong) || any(df[[variable]] >= more_strong)) {
      TRUE
    } else FALSE

  } else if (test == "less_more_weak") {
    less_weak <- get_outlier(df = df, variable = variable, type = "weak_lower")
    more_weak <- get_outlier(df = df, variable = variable, type = "weak_upper")

    if (any(df[[variable]] <= less_weak) || any(df[[variable]] >= more_weak)) {
      TRUE
    } else FALSE
  }
}




#' Add Aggregate Summary.
#'
#' @description Add an aggregate summary to row, column or both.
#'
#' @param df data.frame
#' @param fun character,closure: The summary function to use.
#' @param where character: Where to add the summary either 'row', 'column' or
#' 'both'
#'
#' @return tabyl with summary corresponding to the  where argument.
#' @export
#'
#' @examples
adorn_aggregate <- function(df, fun, where = "both") {

  col_id <- names(df)[1]

  if (is.character(fun)) {
    s_fun <- fun
    fun <- rlang::as_closure(fun)

  } else if (!is.character(fun)) {
    s_fun <- deparse(substitute(fun))
  }

  agg_name <- c("min" = ".Minimum", "max" = ".Maximum", "sd" = ".SD",
                "mean" = ".Average", "median" = ".Median", "sum" = ".Total")
  agg_name <- agg_name[[s_fun]]

  add_to_row <- dplyr::summarise(df, dplyr::across(-tidyselect::all_of(col_id), fun, na.rm = TRUE))|>
    dplyr::mutate("{col_id}" := agg_name, .before = 1)

  add_to_col <- dplyr::rowwise(df) |>
    dplyr::transmute("{agg_name}" := fun(dplyr::c_across(-tidyselect::all_of(col_id)), na.rm = TRUE)) |>
    dplyr::ungroup()

  if (where == "both") {
    df |>
      dplyr::bind_rows(add_to_row) |>
      dplyr::bind_cols(dplyr::add_row(add_to_col, "{agg_name}" := 0))

  } else if (where == "row") {
    dplyr::bind_rows(df, add_to_row)

  } else if (where == "column") {
    dplyr::bind_cols(df, add_to_col)
  }
}




#' Reorder Plot Output for Each Facets
#'
#' @description Extend tidytext::scale_x_reordered by adding a stringr::str_trunc
#'
#' @param ... see ?tidytext::scale_x_reordered
#' @param sep see ?tidytext::scale_x_reordered
#' @param wd numeric: see ?stringr::str_trunc
#' @param use_warp logical, use the scale_x_discrete_wrap function else scale_x_discrete
#' @param df,chr_var,min_chr, if use_warp = TRUE, argument for scale_x_discrete_wrap
#' @param ... other arguments passed to ggplot2::scale_x_discrete function.
#'
#' @return ggplot object.
#' @export
#'
#' @examples
scale_X_reordered <- function(..., sep = "___", wd, df, chr_var, min_chr, use_warp = FALSE) {

  cl_string <- function(x) {
    gsub(paste0(sep, ".+$"), "", x) |>
      stringr::str_trunc(width = wd, side = "right")
  }

  if (use_warp) {
    scale_x_discrete_wrap(df, chr_var, min_chr, labels = \(.x) cl_string(.x), ...)

  } else {
    ggplot2::scale_x_discrete(labels = \(.x) cl_string(.x), ...)
  }
}




#' Character and Numeric Variable Segment Summary
#'
#' @param df data.frame
#' @param num_var numeric: A variable from the data.
#' @param chr_var character: A variable from the data.
#' @param s_fun character: A quoted summary function. optional.
#' @param n_cat numeric: Number of unique character variable values to plot.
#' @param output_type character: The type of output to return either a 'plot'
#' or a 'table'
#'
#' @return if argument output_type == 'table' a data.frame else a ggplot object.
#' @import is_with_outlier, adorn_aggregate, plot_labels.
#' @export
#'
#' @examples
chr_num_variable <- function(df,
                             num_var,
                             chr_var,
                             s_fun = "No Selection",
                             n_cat = 10,
                             output_type = "plot") {

  output_type <- match.arg(output_type, c("table", "plot"))

  outlier_presence <- is_with_outlier(df = df, variable = num_var, test = "less_more_strong")
  if (isTRUE(outlier_presence) && s_fun == "No Selection") {
    s_fun <- "median"
  } else if (isFALSE(outlier_presence) && s_fun == "No Selection") {
    s_fun <- "mean"
  }
  fun <- rlang::as_closure(s_fun)

  # General Table -------------------------------------------------------------|
  f_tbl <- dplyr::group_by(df, .segment, .data[[chr_var]]) |>
    dplyr::summarise("{s_fun}" := fun(.data[[num_var]]), .groups = "drop")

  if (output_type == "table") {
    tidyr::pivot_wider(f_tbl, id_cols = .data[[chr_var]],
                       names_from  = .segment,
                       values_from = tidyselect::all_of(s_fun)) |>
      adorn_aggregate(fun = s_fun)

  } else if (output_type == "plot") {
    len_unique_chr <- length(unique(f_tbl[[chr_var]]))

    agg_lb <- plot_labels(s_fun, stat_label)
    num_lb <- plot_labels(num_var)

    if (len_unique_chr <= 10) {
      chr_lb <- plot_labels(chr_var)
      chr_tl <- chr_lb
    } else {
      chr_lb <- plot_labels(chr_var)
      chr_tl <- paste("Top", n_cat, chr_lb)
    }

    if (len_unique_chr < 6) {
      f_plt <- f_tbl |>
        ggplot2::ggplot(ggplot2::aes(x = tidytext::reorder_within(.data[[chr_var]],
                                                                  .data[[s_fun]],
                                                                  .segment),
                                     y = .data[[s_fun]],
                                     fill = .segment)) +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text.y = ggplot2::element_text(size = 13),
                       plot.background = ggplot2::element_rect(fill = plt_plot_BC,
                                                               color = plt_plot_BC))

    } else {
      f_tbl <- lump_large_chr_count(f_tbl, chr_var, n_max = n_cat)

      f_plt <- f_tbl |>
        ggplot2::ggplot(ggplot2::aes(x = tidytext::reorder_within(.data[[chr_var]],
                                                                  .data[[s_fun]],
                                                                  .segment),
                                     y = .data[[s_fun]],
                                     fill = .segment)) +
        ggplot2::coord_flip() +
        ggplot2::theme_minimal() +
        ggplot2::theme(axis.text = ggplot2::element_text(size = 13),
                       plot.background = ggplot2::element_rect(fill = plt_plot_BC,
                                                               color = plt_plot_BC))
    }
    f_plt <- f_plt +
      ggplot2::geom_col(show.legend = FALSE) +
      ggplot2::facet_wrap(ggplot2::vars(.segment), scales = "free") +
      ggplot2::scale_fill_manual(values = plt_clr$dash)

    if (len_unique_chr < 6) {
      f_plt <- f_plt +
        scale_X_reordered(wd = 15, df = f_tbl, chr_var = chr_var, min_chr = 13, use_warp = FALSE)
    } else {
      f_plt <- f_plt + scale_X_reordered(wd = 15, use_warp = FALSE)
    }

    f_plt +
      ggplot2::scale_y_continuous(labels = scales::comma_format(1)) +
      ggplot2::labs(x = chr_lb, y = NULL,
                    title = paste(agg_lb, num_lb, "By", chr_tl, "In Each Segment"))
  }
}




#' Create a United Variable Data Type.
#'
#' @param df data.frame
#' @param var1 A variable from the data frame.
#' @param var2 A variable from the data frame.
#'
#' @return
#' @export
#'
#' @examples
unite_dtypes <- function(df, var1, var2) {
  chr <- c("ordered", "factor", "character")
  num <- c("integer", "double", "numeric")

  get_dtype <- function(var) {
    if (any(class(df[[var]]) %in% chr)) {
      out_var <- "character"
    } else if (any(class(df[[var]]) %in% num)) {
      out_var <- "numeric"
    }
    return(out_var)
  }

  if (missing(var2)) {
    out_var1 <- get_dtype(var1)
    output <- out_var1

  } else {
    out_var1 <- get_dtype(var1)
    out_var2 <- get_dtype(var2)
    output <- c(out_var1, out_var2)
    names(output) <- c(var1, var2)
  }
  return(output)
}




#' Wrapper Of Segment Summary
#'
#' @param wr_df data.frame
#' @param var_one character/numeric: A variable from the data.
#' @param var_two character/numeric: A variable from the data.
#' @param n_cat  numeric: Number of unique character variable values to plot.
#' @param str_fun character: An quoted aggregate function.
#'
#' @return A list containing data.frame and plots
#' @import unite_dtypes, one_chr_variable, one_num_variable, two_chr_variable,
#' two_num_variable, chr_num_variable.
#' @export
#'
#' @examples
num_chr_wrapper <- function(wr_df,
                            var_one,
                            var_two = "No Selection",
                            n_cat = 10,
                            str_fun = "No Selection") {

  if (var_two == "No Selection") {
    dtype <- unite_dtypes(wr_df, var_one)

    if (dtype == "character") {
      chr1_plt <- one_chr_variable(df = wr_df, chr_var = var_one, output_type = "plot")
      chr1_tbl <- one_chr_variable(df = wr_df, chr_var = var_one, output_type = "table")

      output <- list(plot = chr1_plt, table = chr1_tbl)

    } else if (dtype == "numeric") {
      num1_plt_at <- one_num_variable(df = wr_df, num_var = var_one, output_type = "plot_at")
      num1_plt_mm <- one_num_variable(df = wr_df, num_var = var_one, output_type = "plot_mm")
      num1_tbl <- one_num_variable(df = wr_df, num_var = var_one, output_type = "table")

      output <- list(num1_plot_at = num1_plt_at,
                     num1_plot_mm = num1_plt_mm,
                     num1_table   = num1_tbl)

    } else {
      shiny::validate(paste("Only numerical and categorical variables",
                            "output can be returned for now."))
    }

  } else {
    dtypes <- unite_dtypes(wr_df, var_one, var_two) |> as.vector()

    if (all(dtypes == "character")) {
      chr2_plt_c <- two_chr_variable(df = wr_df,
                                     chr_var1 = var_one,
                                     chr_var2 = var_two,
                                     n_cat = n_cat,
                                     output_type = "plot_c")
      chr2_plt_p <- two_chr_variable(df = wr_df,
                                     chr_var1 = var_one,
                                     chr_var2 = var_two,
                                     n_cat = n_cat,
                                     output_type = "plot_p")
      chr2_tbl <- two_chr_variable(df = wr_df,
                                   chr_var1 = var_one,
                                   chr_var2 = var_two,
                                   output_type = "table")

      output <- list(chr2_plot_c = chr2_plt_c,
                     chr2_plot_p = chr2_plt_p,
                     chr2_table  = chr2_tbl)

    } else if (all(dtypes == "numeric")) {
      num2_plt_full <- two_num_variable(df = wr_df,
                                        num_varx = var_one,
                                        num_vary = var_two,
                                        output_type = "plot_full")
      num2_plt_facet <- two_num_variable(df = wr_df,
                                         num_varx = var_one,
                                         num_vary = var_two,
                                         output_type = "plot_facet")
      num2_tbl <- two_num_variable(df = wr_df,
                                   num_varx = var_one,
                                   num_vary = var_two,
                                   output_type = "table")

      output <- list(num2_plot_full  = num2_plt_full,
                     num2_plot_facet = num2_plt_facet,
                     num2_table      = num2_tbl)

    } else if (all(c("numeric", "character") %in% dtypes)) {
      get_var_class <- unite_dtypes(wr_df, var_one, var_two)
      chr_var <- get_var_class[which(get_var_class == "character")] |> names()
      num_var <- get_var_class[which(get_var_class == "numeric")] |> names()

      num_chr_plt <- chr_num_variable(df = wr_df,
                                      num_var = num_var,
                                      chr_var = chr_var,
                                      n_cat   = n_cat,
                                      s_fun   = str_fun,
                                      output_type = "plot")
      num_chr_tbl <- chr_num_variable(df = wr_df,
                                      num_var = num_var,
                                      chr_var = chr_var,
                                      s_fun   = str_fun,
                                      output_type = "table")

      output <- list(num_chr_plot  = num_chr_plt,
                     num_chr_table = num_chr_tbl)

    } else {
      shiny::validate(paste("Only numerical and categorical variables",
                            "output can be returned for now."))
    }
  }
  return(output)
}
