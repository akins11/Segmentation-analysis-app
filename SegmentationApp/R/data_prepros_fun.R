#' Variable selection
#'
#' @param df data.frame
#' @param variables character: Variables from the data to use.
#'
#' @return data.frame
#' @export
#'
#' @examples
select_input_vars <- function(df, variables) {
  if (length(variables) < 2) {
    shiny::validate("A minimum of two variables must be selected to continue")
  }
  
  f_tbl <- df |>
    dplyr::select(tidyselect::all_of(variables))
  
  return(f_tbl)
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
  df <- df[vapply(df, \(.x) !lubridate::is.Date(.x), FUN.VALUE =  logical(1))]
  
  
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
    dplyr::select(df, -tidyselect::all_of(id_var))
  } else {
    df
  }
}


