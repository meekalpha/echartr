#' Generate echarts `data` object from a dataframe
#' @export
e_data <- function(df, ...) {
  args <- enexprs(...)
  named_args <- discard_at(args, ~.x == "")
  unnamed_args <- keep_at(args, ~.x == "")

  if (length(unnamed_args) > 0 && "value" %in% names(named_args)) {
    stop("Must only use one of unnamed arguments or value argument")
  }
  if (length(unnamed_args) == 0 && !"value" %in% names(named_args)) {
    stop("Must provide dimensions as either unnamed arguments or value argument")
  }

  if (length(named_args) == 0 & length(unnamed_args) > 0) {
    zip(!!!map(unnamed_args, ~sub_values2(df = df, !!.x)))
  } else {
    if (length(unnamed_args) > 0) {
      named_args$value <- expr_c(!!!unnamed_args)
    }
    zip(!!!sub_args(df, !!!named_args))
  }
}

#' Generate a list of series from a dataframe
e_series <- function(df, ...) {
  args <- enexprs(...)
  named_args <- discard_at(args, ~.x == "")
  unnamed_args <- keep_at(args, ~.x == "")

  series_args <- named_args |> discard_at(~.x == "data")

  # Split the dataframe based on the serie-level args
  cols <- series_args |> map(~sub_values(df, !!.x)) |> map(unlist) # TODO support list types

  df |>
    mutate(!!!cols) |>
    as_tibble() |>
    group_by(!!!syms(names(cols))) |>
    group_split() |>
    map(function(df) {
      serie <- names(cols) |>
        purrr::set_names() |>
        map(function(.x) { df[[.x]][[1]] })
      list2(!!!serie, data = e_data(df, !!!unnamed_args)) # TODO use named_args$data

    })
}
