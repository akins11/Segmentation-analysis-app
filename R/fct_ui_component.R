#' Optimal number of cluster ui card.
#'
#' @param opt_cluster number of cluster.
#'
#' @return
#' @export
#'
suggested_optimal_cluster_card <- function(opt_cluster = NULL) {
  if (!is.null(opt_cluster)) {
    shiny::div(
      class = "card border-0 shadow-none",
      style = "background-color: #F8F9FA;",

      shiny::div(
        class = "card-body text-center fs-4",

        shiny::p(
          "Suggested",

          shiny::span(class = "fs-3", "Optimal"),

          "number of clusters is",

          shiny::span(
            class = "fw-bold fs-3 border rounded px-4",
            style = "background-color: #E8E8E8; color: #000000",
            opt_cluster
          )
        )
      )
    )
  }
}



#' UI loading screen
#'
#' @param sp_color the color of the loading image.
#'
#' @return
#' @export
#'
ui_spinner <- function(ui_element, sp_color = spinner_color) {
  shinycssloaders::withSpinner(
    ui_element = ui_element,
    type = 4,
    color = sp_color,
    color.background = "white"
  )
}
