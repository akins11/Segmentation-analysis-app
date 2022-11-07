#' Check for missing values
#'
#' @param df user uploaded data
#'
#' @return Boolean
#' @export
#'
#' @examples
check_for_missing_values <- function(df) {
  if (is.data.frame(df)) {
    vapply(df, \(.x) sum(is.na(.x)) > 1, FUN.VALUE = logical(1)) |>
      any()
  }
}



#' Drop missing values
#'
#' @param df user uploaded data
#' @param missing_threshold The percentage of missing values a variable can have
#' to be removed.
#'
#' @return data frame
#' @export
#'
#' @examples
drop_all_missing_values <- function(df, missing_threshold = 75) {
  vars_with_large_NAs <- vapply(df,
                                \(.x) (sum(is.na(.x))/nrow(df))*100 >= missing_threshold,
                                FUN.VALUE = logical(1))

  f_tbl <- df[!vars_with_large_NAs]
  f_tbl[complete.cases(f_tbl), ]
}




#' Check if data is Usable
#'
#' @param clean_df  A data.frame without any missing value
#' @param original_df_nrows Number of rows of the original dataset
#'
#' @return
#' @export
#'
#' @examples
check_usable_data_frame <- function(clean_df, original_df_nrows) {
  if (!is.data.frame(clean_df)) {
    FALSE
  } else if (is.data.frame(clean_df)) {
    percent_rows <- (nrow(clean_df) / original_df_nrows)*100
    if (percent_rows <= 10) {
      if (nrow(clean_df) < 30) {
        FALSE
      } else {
        TRUE
      }
    } else if (ncol(clean_df) < 2) {
      FALSE
    } else {
      TRUE
    }
  }
}




#' Variable selection
#'
#' @param df data.frame
#' @param variables character: Variables from the data.
#'
#' @return data.frame
#' @export
#'
#' @examples
select_input_vars <- function(df, variables) {
  if (length(variables) < 2) {
    shiny::validate("A minimum of two variables must be selected to continue")
  }

  if (any(variables %in% names(df))) {
    dplyr::select(df, tidyselect::all_of(variables))

  } else {
    dplyr::select(df, 1)
  }
}


factor_to_numeric <- function(x, lowest = NULL) {
  if (is.numeric(x)) {
    return(x)
  }
  if (is.logical(x)) {
    return(as.numeric(x))
  }
  if (anyNA(suppressWarnings(as.numeric(as.character(stats::na.omit(x)))))) {
    if (is.character(x)) {
      x <- as.factor(x)
    }
    x <- droplevels(x)
    levels(x) <- 1:nlevels(x)
  }
  out <- as.numeric(as.character(x))
  if (!is.null(lowest)) {
    difference <- min(out) - lowest
    out <- out - difference
  }
  return(out)
}

factor_to_dummy <- function(x) {
  if (is.numeric(x)) {
    return(x)
  }
  values <- if (is.factor(x)) {
    levels(x)
  }
  else {
    stats::na.omit(unique(x))
  }
  dummy <- as.data.frame(do.call(cbind, lapply(values, function(i) {
    out <- rep(0, length(x))
    out[is.na(x)] <- NA
    out[x == i] <- 1
    out
  })))
  colnames(dummy) <- values
  return(dummy)
}


#' Data Preparation and cleaning
#'
#' @param df data.frame
#' @param include_factors logical: Whether to include character variables.
#' @param standardize logical: Whether to Scale the data or not.
#' @param preprocess logical: Whether to clean/prepare the data for clustering.
#' @param ... Additional arguments passed to datawizard::standardize
#'
#' @return data.frame
#' @import factor_to_numeric, factor_to_dummy.
#' @export
#'
#' @examples
prepare_cluster_data <- function(df,
                                 include_factors = FALSE,
                                 standardize = FALSE,
                                 preprocess = TRUE,
                                 ...) {
  if (preprocess == FALSE) {
    return(df)
  }

  # drop dates ----------------------------------------------------------------|
  is.date <- function(x) {
    inherits(x, c("Date", "POSIXt"))
  }
  df <- df[vapply(df, \(.x) !is.date(.x), FUN.VALUE = logical(1))]


  if (include_factors) {
    # -------------------------------------------------------------------------|
    fct <- lapply(df, \(.x) is.character(.x) | is.factor(.x) | is.ordered(.x)) |>
      unlist()
    same_len <- lapply(df[fct], \(.x) length(unique(.x))) |> unlist()

    valid_chr_vars <- same_len[which(same_len != nrow(df))] |> names()
    num_nm <- df[sapply(df, is.numeric)] |> names()

    f_tbl <- df[c(num_nm, valid_chr_vars)]
    # -------------------------------------------------------------------------|

    factors <- sapply(f_tbl, is.ordered)
    if (any(factors)) {
      f_tbl[factors] <- sapply(f_tbl[factors], factor_to_numeric)
    }
    factors <- sapply(f_tbl, \(.x) is.character(.x) | is.factor(.x))
    if (any(factors)) {
      dummies <- lapply(f_tbl[factors], factor_to_dummy)
      f_tbl <- cbind(f_tbl[!factors], dummies)
    }
  } else {
    f_tbl <- df[sapply(df, is.numeric)]
  }

  f_tbl <- stats::na.omit(f_tbl)
  if (standardize == TRUE) {
    f_tbl <- datawizard::standardize(f_tbl, ...)
  }
  return(f_tbl)
}




#' Send data to cluster algorithm
#'
#' @param df data.frame
#' @param id_var The unique data identification variable.
#' @param standardize logical: Whether to standardize the data.
#'
#' @return data.frame with unique row ids.
#' @import prepare_cluster_data
#' @export
#'
#' @examples
push_to_cluster_alg <- function(df,
                                id_var = "No Selection",
                                standardize) {
  if (id_var != "No Selection") {
    record_id <- df[[id_var]]
    df <- df[which(names(df) != id_var)]
  }

  # clean data for clustering -------------------------------------------------|
  no_chr <- sum(sapply(df, \(.x) is.character(.x) | is.factor(.x) | is.ordered(.x)))
  include_factors <- ifelse(no_chr != 0, TRUE, FALSE)

  f_tbl <- prepare_cluster_data(df = df,
                                include_factors = include_factors,
                                standardize = standardize)

  if (id_var != "No Selection") {
    if (length(unique(record_id)) != nrow(f_tbl)) {
      shiny::validate("selected `Id variable` must be unique for all rows")
    }
  }

  if (id_var == "No Selection") {
    if (isFALSE(tibble::has_rownames(f_tbl))) {
      f_tbl |>
        dplyr::mutate(row_id = paste0("id", seq_len(nrow(f_tbl))), .before = 1) |>
        tibble::column_to_rownames("row_id")
    }
  } else {
    if (isFALSE(tibble::has_rownames(f_tbl))) {
      dplyr::mutate(f_tbl, row_id = record_id) |>
        tibble::column_to_rownames("row_id")
    }
  }
}




#' Title
#'
#' @description Drop id variable from the original data if available.
#'
#' @param df data.frame
#' @param id_var character: A id variable to drop if available i.e not "No Selection"
#'
#' @return
#' @export
#'
#' @examples
drop_data_id <- function(df, id_var = "No Selection") {
  if (id_var != "No Selection") {
    if (id_var %in% names(df)) {
      dplyr::select(df, -tidyselect::all_of(id_var)) |>
        stats::na.omit()

    } else {
      stats::na.omit(df)
    }

  } else {
    df
  }
}


