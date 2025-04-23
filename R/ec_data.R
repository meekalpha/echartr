
#' Generate echarts `data` list-tree object from specification dataframe
#' @keywords internal
#' @importFrom rlang .data
ec_data_ <- function(spec) {

  if ("value" %in% colnames(spec)) {
    warning("Data values provided via 'values' will not be used")
  }

  if (length(spec$datapoint) == 0) {
    spec$datapoint
  } else if (ncol(spec) == 1) {
    if (length(spec$datapoint[[1]]) == 1) {
      unlist(spec$datapoint)
    } else {
      spec$datapoint
    }
  } else {
    spec |>
      dplyr::mutate(value = .data$datapoint) |>
      dplyr::select(-.data$datapoint) |>
      purrr::transpose()
  }
}

#' Generate echarts `data` list-tree object from a dataframe
#' @param df data frame that can be referenced via ... arguments
#' @param ... Expressions that can be evaluated to generate the list-tree
#' @export
ec_data <- function(df, ...) {
  ec_data_(build_spec(df, ...))
}
