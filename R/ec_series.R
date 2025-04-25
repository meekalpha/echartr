#' Create a list of series based on specification data frame
#' @keywords internal
#' @importFrom dplyr all_of
ec_series_ <- function(spec) {

  serie_cols <- setdiff(names(spec), c("data", "datapoint"))

  x <- spec |>
    dplyr::group_by(!!!rlang::syms(serie_cols))

  # Summarise to one row per series
  series <- spec |>
    dplyr::group_by(!!!rlang::syms(serie_cols)) |>
    dplyr::group_split() |>
    purrr::map(~{
      .x |>
        dplyr::summarise(
          .by = all_of(serie_cols),
          data = list(.x |> spec_zoom("data") |> ec_data_())
        )
    }) |>
    dplyr::bind_rows()

  # Avoid any reordering of grouping columns
  spec |>
    dplyr::summarise(.by = all_of(serie_cols)) |> # Using .by avoids re-ordering
    dplyr::left_join(series, by = serie_cols) |>
    purrr::transpose()
}

#' Generate a list of series from a dataframe
#'
#' @param df A dataframe that can be referenced by all other arguments
#' @param ... data dimensions and series attributes
#'
#' Unnamed arguments are used as data dimensions in the order provided.
#' Named arguments should be valid series attributes.
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
  structure(ec_series_(build_spec(df, type = type, ...)), class = "ec_object")
}

#' Line series
#'
#' https://echarts.apache.org/en/option.html
#'
#' Adds option attribute `type = "line"` and will give an error if `y` is not provided.
#'
#' @param df A dataframe that can be referenced via ... arguments
#' @param ... Series attributes
#' @param x The x-axis variable
#' @param y The y-axis variable
#'
#' @seealso [ec_series()]
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
#' https://echarts.apache.org/en/option.html#series-bar
#'
#' Generates a single bar series or list of series from a dataframe.
#'
#' @param df A dataframe that can be referenced via ... arguments
#' @param x The x-axis variable
#' @param y The y-axis variable
#' @param ... Additional arguments to be passed to the series
#' @seealso [ec_series()]
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
#' https://echarts.apache.org/en/option.html#series-pie
#'
#' @param df A dataframe that can be referenced by all other arguments
#' @param value expression refering to `df` that gives the value to plot
#' @param ... additional expressions providing series attributes and additional dimensions
#'
#' Unnamed arguments will be used as additional data dimensions in the order provided.
#' @seealso [ec_series()]
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
#' @seealso [ec_series()]
#' @param df
#' @export
ec_scatter <- function(df, x, y, ...) {
  if (missing(y)) {
    stop("`y` argument is required")
  }
  if (missing(x)) {
    ec_series(df, type = "scatter", !!rlang::enexpr(y), ...)
  } else {
    ec_series(df, type = "scatter", !!rlang::enexpr(x), !!rlang::enexpr(y), ...)
  }
}
