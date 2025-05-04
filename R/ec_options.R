#' Create a list of options based on specification data frame
#' @keywords internal
#' @importFrom dplyr all_of
ec_options_ <- function(spec) {

  option_cols <- setdiff(names(spec), c("series", "datapoint"))

  # Summarise to one row per series
  series <- spec |>
    dplyr::group_by(!!!rlang::syms(option_cols)) |>
    dplyr::group_split() |>
    purrr::map(~{
      .x |>
        dplyr::summarise(
          .by = all_of(option_cols),
          series = list(.x |> spec_zoom("series") |> ec_series_())
        )
    }) |>
    dplyr::bind_rows()

  # Avoid any reordering of grouping columns
  if (purrr::is_empty(option_cols)) {
    spec |>
      dplyr::summarise(.by = all_of(option_cols)) |> # Using .by avoids re-ordering
      dplyr::bind_cols(series) |>
      purrr::transpose()
  } else {
    spec |>
      dplyr::summarise(.by = all_of(option_cols)) |> # Using .by avoids re-ordering
      dplyr::left_join(series, by = option_cols) |>
      purrr::transpose()
  }
}

#' Generate a list of options from a dataframe
#'
#' @param df a dataframe that can be referenced by all other arguments
#' @param ... data dimensions and option attributes.
#'
#' Unnamed arguments are used as data dimensions.
#' Named arguments should be valid option attributes.
#'
#' @export
ec_options <- function(df, ...) {
  structure(ec_options_(build_spec(df, ...)), class = "ec_object")
}
