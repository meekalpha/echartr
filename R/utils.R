
#' Evaluate quosures provided to ... for each for in df
row_eval <- function(df, ...) {
  args <- rlang::enquos(...)

  for (i in seq_along(args)) {
    if (names(args)[[i]] == "") {
      names(args)[i] <- paste0("unnamed", i)
    }
  }

  res <- purrr::map_dfr(seq_len(nrow(df)), function(i) {
    row <- as.list(df[i, ])
    purrr::imap_dfc(args, function(arg, name) {
      value <- rlang::eval_tidy(
        rlang::get_expr(arg),
        env = list2env(row, parent = rlang::get_env(arg))
      )
      tibble::tibble(
        .name_repair = "minimal",
        !!name := if (is.list(value)) list(value) else value
      )
    })
  })

  unnamed_cols <- syms(res |> select(starts_with("unnamed")) |> colnames())

  res |>
    rowwise() |>
    mutate(unnamed = list(rlang::list2(!!!unnamed_cols)), .keep = "unused") |>
    ungroup()
}
