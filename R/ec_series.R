#' Create a list of series based on specification data frame
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
#'
#' @seealso [ec_scatter()]
#' @seealso [ec_line()]
#' @seealso [ec_bar()]
#' @seealso [ec_pie()]
#'
#' @export
ec_series <- function(df, ..., type) {
  if (missing(type)) {
    stop("`type` argument is required")
  }
  structure(ec_series_(build_spec(df, ...)), class = "ec_object")
}

#' Line series
#'
#' @examples
#'
#' # Including x-value in series data
#' data <- tibble::tibble(x = 1:5, y = rnorm(5))
#' series <- ec_line(data, x, y)
#'
#' echartr(option = list(
#'   xAxis = list(type = "value"),
#'   yAxis = list(type = "value"),
#'   legend = list(show = TRUE),
#'   series = series
#' ))
#'
#' # Including x-value in axis data - requires category type axis
#' data <- tibble::tibble(x = 1:5, y = rnorm(5))
#' series <- ec_line(data, y = y)
#'
#' echartr(option = list(
#'   xAxis = list(type = "category", data = data$x),
#'   yAxis = list(type = "value"),
#'   legend = list(show = TRUE),
#'   series = series
#' ))
#'
#' @export
ec_line <- function(df, x, y, ...) {
  if (missing(y)) {
    stop("`y` argument is required")
  }
  if (missing(x)) {
    ec_series(df, type = "line", !!rlang::enexpr(y), ...)
  } else {
    ec_series(df, type = "line", !!rlang::enexpr(x), !!rlang::enexpr(y), ...)
  }
}

#' Bar series
#'
#' @examples
#'
#' # Including x-value in series data
#' data <- tibble::tibble(x = 1:5, y = rnorm(5))
#' series <- ec_bar(data, x, y)
#'
#' echartr(option = list(
#'   xAxis = list(type = "value"),
#'   yAxis = list(type = "value"),
#'   legend = list(show = TRUE),
#'   series = series
#' ))
#'
#' # Including x-value in axis data - requires category type axis
#' data <- tibble::tibble(x = 1:5, y = rnorm(5))
#' series <- ec_bar(data, y = y)
#'
#' echartr(option = list(
#'   xAxis = list(type = "category", data = data$x),
#'   yAxis = list(type = "value"),
#'   legend = list(show = TRUE),
#'   series = series
#' ))
#'
#' @export
ec_bar <- function(df, x, y, ...) {
  if (missing(y)) {
    stop("`y` argument is required")
  }
  if (missing(x)) {
    ec_series(df, type = "bar", !!rlang::enexpr(y), ...)
  } else {
    ec_series(df, type = "bar", !!rlang::enexpr(x), !!rlang::enexpr(y), ...)
  }
}

#' Pie series
#'
#' @examples
#'
#' data <- tibble::tibble(name = LETTERS[1:5], value = rnorm(5))
#' series <- ec_pie(data, value, data = list(name = name))
#'
#' echartr(option = list(
#'   series = series
#' ))
#'
#' @export
ec_pie <- function(df, value, ...) {
  if (missing(value)) {
    stop("`value` argument is required")
  }
  ec_series(df, type = "pie", !!rlang::enexpr(value), ...)
}

#' Scatter series
#'
#' Generates a single scatter series or list of series from a dataframe.
#'
#' https://echarts.apache.org/en/option.html#series-scatter
#'
#' @examples
#'
#' # Basic unnamed series
#' data <- tibble::tibble(x = rnorm(100), y = rnorm(100))
#' ec_scatter(data, x, y)
#'
#' # Multiple series
#' data <- tibble::tibble(
#'   x = rnorm(100),
#'   y = rnorm(100),
#'   serie = sample(letters[1:3], 100, replace = TRUE)
#' )
#' ec_scatter(data, x, y, name = serie)
#'
#' # Display series in a plot
#' data <- tibble::tibble(
#'   x = rnorm(100),
#'   y = rnorm(100),
#'   serie = sample(letters[1:3], 100, replace = TRUE)
#' )
#' series <- ec_scatter(data, x, y, name = serie)
#'
#' echartr(option = list(
#'   xAxis = list(type = "value"),
#'   yAxis = list(type = "value"),
#'   legend = list(show = TRUE),
#'   series = series
#' ))
#'
#' @export
ec_scatter <- function(df, x, y, ...) {
  ec_series(df, type = "scatter", !!rlang::enexpr(x), !!rlang::enexpr(y), ...)
}
