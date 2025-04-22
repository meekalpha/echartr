#' Move one level 'deeper' into the specification
#' @keywords internal
#' TODO: support unnamed stuff inside row (maybe)
spec_zoom <- function(spec, col) {
  res <- spec |>
    dplyr::select(datapoint)

  if (col %in% colnames(spec)) {
    res <- spec |>
      dplyr::pull(!!rlang::sym(col)) |>
      purrr::map_depth(2, list) |>
      purrr::reduce(dplyr::bind_rows) |>
      dplyr::bind_cols(res)
  }
  res
}

#' Convert function args to a dataframe
#' @keywords internal
#' @param ... A combination of named arguments, which will each become a column,
#'            and unnamed arguments, which will be combined into a column called `datapoint`
build_spec <- function(...) {

  # TODO: handle NULL args..

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
