
ec_options_ <- function(spec) {
  x <- spec |>
    group_by(!!!syms(setdiff(names(spec), c("series", "unnamed"))))

  series <- x |> group_split() |> map(~spec_zoom(.x, "series")) |> map(ec_series_)

  x |> summarise(.groups = "drop") |> mutate(series = series) |> transpose()
}

#' @export
ec_options <- function(df, ...) {
  ec_options_(row_eval(df, ...))
}
