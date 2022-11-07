#' Clean User Supplied Segments.
#'
#'@description vectorize user supplied segments.
#'
#' @param x character: A string of all segments separated by a comma or space.
#'
#' @return vector of supplied segments.
#' @export
#'
#' @examples
get_user_segments <- function(x) {

  supplied_segment <- strsplit(x, ",")  |> unlist() |> as.character() |> trimws()
  supplied_segment[grep("[a-zA-Z]", supplied_segment)]
}
#(,)|(\\s)


#' Segment assignment with aggregate summary.
#'
#'@description Assign segment to a clustered data.frame.
#'
#' @param df data.frame: A df with a variable '.cluster' in it.
#' @param num_var numeric: A variable to use in calculating each cluster weight
#' @param str_fun closure: Function to use when aggregating.
#' @param segment character/numeric: Segments sorted from the best to the least
#' @param drop_cluster logical: Whether to drop the '.cluster' variable.
#'
#' @return A data.frame/tibble with a .segment variable.
#' @export
#'
#' @examples
assign_seg_num_var <- function(df,
                               num_var,
                               str_fun = "mean",
                               segment,
                               drop_cluster = FALSE) {

  if (length(segment) != length(unique(df$.cluster))) {
    shiny::validate("!!!")
  }

  chr_segment <- ifelse(!is.character(segment),
                        list(as.character(segment)), list(segment))[[1]]


  fun <- rlang::as_closure(str_fun)
  # All clusters in the data will be grouped together, then then the numeric
  # variable supplied will be summarise by the supplied aggregate function.
  # Then the supplied segment value. well replace each cluster based on the sorted
  # summarised values.
  deframe_cluster <- dplyr::group_by(df, .cluster) |>
    dplyr::summarise("{str_fun}" := fun(.data[[num_var]], na.rm = TRUE)) |>
    dplyr::arrange(desc(.data[[str_fun]])) |>
    tibble::deframe()


  recode <- names(deframe_cluster)
  names(recode) <- chr_segment

  f_tbl <- df |>
    dplyr::mutate(.segment = forcats::fct_recode(.cluster, !!!recode),
                  .segment = as.character(.segment),
                  .segment = ifelse(is.numeric(segment),
                                    list(as.numeric(.segment)),
                                    list(.segment))[[1]])
  if (isTRUE(drop_cluster)) {
    dplyr::select(f_tbl, -.cluster)
  } else {
    f_tbl
  }
}




#' Assignment Segment Manually.
#'
#' @param df data.frame: A df with a variable '.cluster' in it.
#' @param segment character/numeric: Segments sorted from 1 to number of cluster.
#' @param drop_cluster logical: Whether to drop the '.cluster' variable.
#'
#' @return A data.frame/tibble with a .segment variable.
#' @export
#'
#' @examples
assign_seg_manually <- function(df, segment, drop_cluster = FALSE) {

  number_cluster <- length(unique(df$.cluster))

  if (length(segment) != number_cluster) {
    shiny::validate(paste("The number segments supplied and the number of",
                          "available clusters must be the same."))
  }
  if (!is.character(segment)) {
    chr_segment <- as.character(segment)
  }

  rc_levels <- setNames(as.character(seq_len(number_cluster)), segment)

  f_tbl <- df |>
    dplyr::mutate(.segment = forcats::fct_recode(.cluster, !!!rc_levels))

  if (isTRUE(drop_cluster)) {
    f_tbl <- dplyr::select(f_tbl, -.cluster)
  }

  return(f_tbl)
}



get_num_clusters <- function(df) {
  length(unique(df$.cluster))
}
