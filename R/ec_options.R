
ec_options_ <- function(spec) {

  option_cols <- setdiff(names(spec), c("series", "datapoint"))

  x <- spec |>
    dplyr::group_by(!!!rlang::syms(option_cols))

  series <- x |>
    dplyr::group_split() |>
    purrr::map(~spec_zoom(.x, "series")) |>
    purrr::map(ec_series_)

  x |>
    dplyr::summarise(.groups = "drop") |>
    dplyr::mutate(series = series) |>
    purrr::transpose()
}

#' @export
ec_options <- function(df, ...) {
  structure(ec_options_(build_spec(df, ...)), class = "ec_object")
}
