#' Clean data column Names
#'
#' @description Clean the column names for rendered table.
#'
#' @param df data.frame
#' @param relocate logical: change the position of a variable to the first.
#' @param include character: The variable position to change.
#'
#' @return A data.frame/tibble/tabyl with cleaned column names.
#' @export
#'
#' @examples
clean_df_names <- function(df, relocate = TRUE, include) {

  f_tbl <- janitor::clean_names(df) |>
    dplyr::rename_with(
      ~stringr::str_to_title(stringr::str_replace_all(.x, "_", " "))
    )

  agg <- c(".Total", ".Average", ".Median", ".Minimum", ".Maximum")
  if (any(agg %in% names(df))) {
    keep_name <- agg[which(agg %in% names(df))]

    f_tbl <- dplyr::rename(f_tbl, "{keep_name}" := ncol(f_tbl))
  }

  if (isTRUE(relocate) && !missing(include)) {
    var_name <- stringr::str_to_title(include)
    location <- which(names(f_tbl) == var_name)

    if (location != 1) {
      f_tbl <- dplyr::relocate(f_tbl, .data[[var_name]], .before = 1)
    }
  }
  return(f_tbl)
}




#' Wrapper For Reacteable column Names
#'
#' @description Clean the names of a data frame or a list of data.frame
#'
#' @param x data.frame|list:
#' @param relocate see clean_df_names function.
#' @param include see clean_df_names function.
#'
#' @return A data.frame/tibble/tabyl with cleaned column names.
#' @export
#'
#' @examples
clean_reactable_names <- function(x, relocate = TRUE, include) {

  if (is.data.frame(x)) {
    clean_df_names(df = x, relocate = relocate, include = include)
  } else if (is.list(x)) {
    lapply(x, \(.x) clean_df_names(.x))
  }
}




#' Reactable Structure For Data Choice.
#'
#' @param df data.frame
#' @param page_size numeric: The number of rows in a  page.
#'
#' @return A reactable HTML widget.
#' @export
#'
#' @examples
upload_df_reactable <- function(df, page_size = 10) {

  reactable::reactable(
    data = df,
    defaultColDef = reactable::colDef(format = reactable::colFormat(digits = 1, separators = TRUE),
                                      na = "–"),
    highlight = TRUE,
    outlined = TRUE,
    compact = TRUE,

    defaultPageSize = page_size,
    fullWidth = TRUE,
    resizable = TRUE,
    paginationType = "jump",

    theme = reactable::reactableTheme(
      backgroundColor = tbl_body_BC,
      borderColor = "#dfe2e5",
      highlightColor = "#f0f5f9",
      headerStyle = list(borderColor = tbl_header_BD,
                         backgroundColor = tbl_header_BC,
                         fontSize = "15px"),
      style = list(fontFamily = tbl_font_family)),

    language = reactable::reactableLang(pageInfo = "{rows} entries",
                                        pagePrevious = "\u276e",
                                        pageNext = "\u276f")
  )
}




#' Upload Data Dimension table
#'
#' @param df data.frame
#'
#' @return
#' @export
#'
#' @examples
get_data_dimension <- function(df) {
  n_row <- nrow(df)
  n_col <- ncol(df)

  data.frame(nm = c(paste("Total Row"), paste("Total Column")),
             vl = c(n_row, n_col)) |>

    reactable::reactable(
      # data = ff,
      theme = reactable::reactableTheme(
        color = "#636363",
        cellPadding = "10px",
        backgroundColor = tbl_body_BC,
        style = list(maxWidth = "300px",
                     borderRadius = "5px"),
        headerStyle = list(color = tbl_body_BC,
                           border = tbl_body_BC)
        ),

      columns = list(
        vl = reactable::colDef(
          style = list(color = "#222222",fontSize = "20px"),
          format = reactable::colFormat(digits = 0, separators = TRUE)
          )
      )
    )
}




#' Reactable Structure For Variable Selection
#'
#' @description reactable format for rendering selected variable & restructured.
#'
#' @param df data.frame
#' @param page_size numeric: The number of rows in a page.
#'
#' @return A reactable HTML widget.
#' @export
#'
#' @examples
var_selection_restructure_rt <- function(df, page_size = 10) {

  reactable::reactable(
    data = df,
    defaultColDef = reactable::colDef(format = reactable::colFormat(digits = 1, separators = TRUE),
                                      na = "–"),
    outlined = TRUE,

    defaultPageSize = page_size,

    fullWidth = TRUE,
    resizable = TRUE,
    paginationType = "simple",

    theme = reactable::reactableTheme(
      backgroundColor = tbl_body_BC,
      borderColor = "#dfe2e5",
      headerStyle = list(borderColor = tbl_header_BD,
                         backgroundColor = tbl_header_BC,
                         fontSize = "15px"),
      style = list(fontFamily = tbl_font_family)),

    language = reactable::reactableLang(pageInfo = "",
                                        pagePrevious = "\u276e",
                                        pageNext = "\u276f")
  )
}




#' Reactable Structure For Cluster or Segment table.
#'
#' @param df data.frame
#' @param include character: The type of column to edit either cluster or segment
#' @param page_size numeric: The number of rows in a  page.
#'
#' @return A reactable HTML widget.
#' @export
#'
#' @examples
cluster_segment_reactable <- function(df, include, page_size = 10) {

  col_preferance <- function(have_borderRight = TRUE) {
    pre_out <- reactable::colDef(
      sortable = TRUE,
      align = "center",
      sticky = "left",
      filterable = TRUE,
      style = list(background = cs_col_BC,
                   color = cs_col_TX,
                   textSize = "12px")
      )

    if (isTRUE(have_borderRight)) {
      pre_out$style$borderRight <- "1px solid #555"
    }
    pre_out
  }

  if (length(include) == 1) {
    if (include == "cluster") {
      edit_col <- list(Cluster = col_preferance())
    } else {
      edit_col <- list(Segment = col_preferance())
    }
  } else {
    edit_col <- list(
      Cluster = col_preferance(),
      Segment = col_preferance(have_borderRight = FALSE)
    )
  }

  reactable::reactable(
    data = df,
    defaultColDef = reactable::colDef(format = reactable::colFormat(digits = 1, separators = TRUE),
                                      na = "–"),
    columns = edit_col,

    sortable = FALSE,
    showSortable = TRUE,
    searchable = TRUE,
    outlined = TRUE,
    fullWidth = TRUE,
    resizable = TRUE,
    highlight = TRUE,

    defaultPageSize = page_size,
    paginationType = "jump",

    theme = reactable::reactableTheme(
      backgroundColor = tbl_body_BC,
      borderColor = "#dfe2e5",
      headerStyle = list(borderColor = tbl_header_BD,
                         backgroundColor = tbl_header_BC,
                         fontSize = "15px"),
      style = list(fontFamily = tbl_font_family)),

    language = reactable::reactableLang(pageInfo = "",
                                        pagePrevious = "\u276e",
                                        pageNext = "\u276f")
  )
}




#' Reactable Structure For a Single Data Frame
#'
#' @description reactable format for rendering single summary table.
#'
#' @param df data.frame
#' @param include character: A column to edit either segment or cluster variable
#' @param page_size numeric: The amount of rows to display.
#'
#' @return A reactable HTML widget.
#' @export
#'
#' @examples
single_reactable <- function(df, include, page_size = 10) {

  if (!missing(include)) {
    col_preferance <- function() {
      reactable::colDef(sortable = TRUE,
             filterable = TRUE,
             align = "center",
             style = list(borderRight = "1px solid #555",
                          background = cs_col_BC,
                          color = cs_col_TX,
                          textSize = "12px") )
    }

    if (include == "cluster") {
      edit_col <- list(Cluster = col_preferance())
    } else {
      edit_col <- list(Segment = col_preferance())
    }

  }

  reactable::reactable(
    data = df,
    defaultColDef = reactable::colDef(na = "–",
                                      format = reactable::colFormat(digits = 1, separators = TRUE)),
    # columns = ifelse(!missing(include), edit_col, NULL),
    columns = if (!missing(include)) edit_col else NULL,

    sortable = FALSE,
    showSortable = TRUE,

    outlined = TRUE,
    fullWidth = TRUE,
    resizable = TRUE,
    highlight = TRUE,

    defaultPageSize = page_size,
    paginationType = "jump",

    theme = reactable::reactableTheme(
      backgroundColor = tbl_body_BC,
      borderColor = "#dfe2e5",
      headerStyle = list(borderColor = tbl_header_BD,
                         backgroundColor = tbl_header_BC,
                         fontSize = "15px"),
      style = list(fontFamily = tbl_font_family)),

    language = reactable::reactableLang(pageInfo = "",
                                        pagePrevious = "\u276e",
                                        pageNext = "\u276f")
  )
}




#' Reactable Structure For Multiple Data Frames
#'
#' @description reactable format for rendering a list of summary tables.
#'
#' @param list_df list: A list With data.frames
#' @param include character: A column to edit either segment or cluster variable.
#'
#' @return A reactable HTML widget.
#' @export
#'
#' @examples
nested_reactable <- function(list_df, include) {

  vars <- janitor::make_clean_names(names(list_df)) |>
    stringr::str_replace_all("_", " ") |>
    stringr::str_to_title()

  # Outer data frame ----------------------------------------------------------|
  f_tbl <- tibble::tibble(Variables = vars)

  # edit cluster | segment ----------------------------------------------------|
  col_preferance <- function() {
    reactable::colDef(
      sortable = TRUE,
      filterable = TRUE,
      align = "center",
      sticky = "left",
      style = list(borderRight = "1px solid #555",
                   background = cs_col_BC,
                   color = cs_col_TX,
                   textSize = "12px") )
  }

  if (include == "cluster") {
    edit_col <- list(Cluster = col_preferance())
  } else {
    edit_col <- list(Segment = col_preferance())
  }

  # ractable output -----------------------------------------------------------|
  reactable::reactable(
    data = f_tbl,
    details = function(index) {
      n_tbl <- list_df[[index]]

      htmltools::div(
        style = "padding: 16px",

        reactable::reactable(
          data = n_tbl,
          outlined = TRUE,
          defaultColDef = reactable::colDef(
            headerStyle = list(background = "#f7f7f8",
                               fontSize = "15px"),
            format = reactable::colFormat(digits = 1, separators = TRUE),
            na = "-"
          ),

          highlight = TRUE,

          columns = edit_col,

          theme = reactable::reactableTheme(style = list(fontFamily = tbl_font_family),
                                            backgroundColor = tbl_body_BC,
                                            headerStyle = list(borderColor = tbl_header_BD,
                                                               backgroundColor = tbl_header_BC,
                                                               fontSize = "13px"),)
        )
      )
    },

    fullWidth = TRUE,
    theme = reactable::reactableTheme(style = list(fontFamily = tbl_font_family),
                                      backgroundColor = tbl_body_BC,
                                      headerStyle = list(borderColor = tbl_header_BD,
                                                         backgroundColor = tbl_header_BC,
                                                         fontSize = "15px"),)
  )
}




#' Wrapper For Single and Multiple Data Frames.
#'
#' @description reactable format for rendering a list or single summary table.
#'
#' @param x data.frame|list
#' @param include see single_reactable and nested_reactable.
#' @param page_size see single_reactable and nested_reactable.
#'
#' @return A reactable HTML widget.
#' @export
#'
#' @examples
summary_reactable <- function(x, include, page_size = 10) {

  if (is.data.frame(x)) {
    if (any(c("Segment", "Cluster") %in% names(x))) {
      single_reactable(df = x, include = include, page_size = page_size)
    } else {
      single_reactable(df = x, page_size = page_size)
    }

  } else if (is.list(x)) {
    nested_reactable(list_df = x, include = include)
  }
}


