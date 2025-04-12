# TODO: Don't use named values when not needed
ec_data_ <- function(spec) {
  if (!"value" %in% names(spec)) {
    spec <- spec |> mutate(value = unnamed)
  }
  spec |> select(-unnamed) |> transpose()
}

#' Generate echarts `data` object from a dataframe
#' @export
ec_data <- function(df, ...) {
  ec_data_(row_eval(df, ...))
}

#' TODO: support unnamed stuff inside row
spec_zoom <- function(spec, col) {
  res <- spec |> select(unnamed)
  if (col %in% colnames(spec)) {
    res <- res |>
      bind_cols(
        spec |> pull(!!sym(col)) |> map_depth(2, list) |> reduce(bind_rows)
      )
  }
  res
}

ec_series_ <- function(spec) {
  x <- spec |>
    group_by(!!!syms(setdiff(names(spec), c("data", "unnamed"))))

  data <- x |> group_split() |> map(~spec_zoom(.x, "data")) |> map(ec_data_)

  x |> summarise(.groups = "drop") |> mutate(data = data) |> transpose()
}

#' Generate a list of series from a dataframe
ec_series <- function(df, ...) {
  ec_series_(row_eval(df, ...))
}

#' @export
ec_line <- function(df, x, y, ...) {
  ec_series(df, type = "line", !!enexpr(x), !!enexpr(y), ...)
}

#' @export
ec_bar <- function(df, y, ...) {
  ec_series(df, type = "bar", !!enexpr(x), !!enexpr(y), ...)
}

#' @export
ec_pie <- function(df, x, y, ...) {
  ec_series(df, type = "pie", !!enexpr(x), !!enexpr(y), ...)
}

#' @export
ec_scatter <- function(df, x, y, ...) {
  ec_series(df, type = "scatter", !!enexpr(x), !!enexpr(y), ...)
}

#' @export
ec_pie <- function(df, x, y, ...) {
  ec_series(df, type = "pie", !!enexpr(x), !!enexpr(y), ...)
}
