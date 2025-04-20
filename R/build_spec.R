build_spec <- function(...) {

  spec <- row_eval(...) |>
    dplyr::rename(datapoint = unnamed)

  if (length(spec$datapoint) == 0 || length(spec$datapoint[[1]]) == 0) {
    warning("No data dimensions specified")
  }

  # Convert datapoint lists to vectors where data is of single type
  n_classes <- spec$datapoint |>
    purrr::flatten() |>
    purrr::map(class) |>
    unique() |>
    length()

  if (n_classes == 1) {
    spec <- spec |>
      dplyr::mutate(datapoint = purrr::map(datapoint, unlist))
  }

  # Convert data to a vector where one dimension of single type
  simple_data <- length(spec$datapoint) > 0 &&
    purrr::every(spec$datapoint, ~!is.list(.x) && length(.x) == 1)

  if (simple_data) {
    spec$datapoint <- unlist(spec$datapoint)
  }

  spec
}
