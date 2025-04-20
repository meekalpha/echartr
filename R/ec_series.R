#' Create a list of series based on spec data frame
ec_series_ <- function(spec) {

  serie_cols <- setdiff(names(spec), c("data", "datapoint"))

  x <- spec |>
    dplyr::group_by(!!!rlang::syms(serie_cols))

  # Split data based on series
  data <- x |>
    dplyr::group_split() |>
    purrr::map(~spec_zoom(.x, "data")) |>
    purrr::map(ec_data_)

  # Join series + data
  x |>
    dplyr::summarise(.groups = "drop") |>
    dplyr::mutate(data = data) |>
    purrr::transpose()
}

#' Generate a list of series from a dataframe
#' @export
ec_series <- function(df, ...) {
  ec_series_(build_spec(df, ...))
}

#' @export
ec_line <- function(df, x, y, ...) {
  ec_series(df, type = "line", !!rlang::enexpr(x), !!rlang::enexpr(y), ...)
}

#' @export
ec_bar <- function(df, y, ...) {
  ec_series(df, type = "bar", !!rlang::enexpr(x), !!rlang::enexpr(y), ...)
}

#' @export
ec_pie <- function(df, x, y, ...) {
  ec_series(df, type = "pie", !!rlang::enexpr(x), !!rlang::enexpr(y), ...)
}

#' @export
ec_scatter <- function(df, x, y, ...) {
  ec_series(df, type = "scatter", !!rlang::enexpr(x), !!rlang::enexpr(y), ...)
}
