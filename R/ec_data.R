# TODO: Don't use named values when not needed
ec_data_ <- function(spec) {

  if ("value" %in% colnames(spec)) {
    stop("Data values should be provided as unnamed arguments rather than using 'value'")
  }

  # Convert datapoint lists to vectors where data is of single type
  n_classes <- spec$unnamed |>
    purrr::flatten() |>
    purrr::map(class) |>
    unique() |>
    length()

  if (n_classes == 1) {
    spec <- spec |>
      dplyr::mutate(unnamed = purrr::map(unnamed, unlist))
  }

  # Convert data to a vector where one dimension of single type
  simple_data <- length(spec$unnamed) > 0 &&
    purrr::every(spec$unnamed, ~!is.list(.x) && length(.x) == 1)

  if (simple_data) {
    spec$unnamed <- unlist(spec$unnamed)
  }

  if (length(spec$unnamed) == 0) {
    spec$unnamed
  }
  else if (ncol(spec) == 1) {
    if (length(spec$unnamed[[1]]) == 1) {
      unlist(spec$unnamed)
    } else {
      spec$unnamed
    }
  } else {
    spec |>
      dplyr::mutate(value = unnamed) |>
      dplyr::select(-unnamed) |>
      purrr::transpose()
  }
}

#' Generate echarts `data` object from a dataframe
#' @export
ec_data <- function(df, ...) {
  ec_data_(row_eval(df, ...))
}
