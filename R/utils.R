
#' Evaluate quosures provided to ... for each row in a dataframe
#' @keywords internal
#' @param df A dataframe to evaluate the quosures against
#' @param ... A combination of named arguments, which will each become a column
#'
#' Unnamed arguments are combined as lists into a column called "unnamed"
#'
#' @importFrom dplyr starts_with
#' @importFrom rlang :=
row_eval <- function(df, ...) {
  args <- rlang::enquos(..., .check_assign = TRUE)
  unnamed <- "echartr.unnamed.arg." # something long to avoid collisions with dot names

  for (i in seq_along(args)) {
    if (names(args)[[i]] == "") {
      names(args)[i] <- paste0(unnamed, i)
    }
  }

  # TODO: Could possibly be achieved more simply with dplyr::rowwise()
  res <- purrr::map_dfr(seq_len(nrow(df)), function(i) {
    row <- df[i, ]
    purrr::imap_dfc(args, function(arg, name) {
      value <- rlang::eval_tidy(
        rlang::get_expr(arg), data = row
      )
      tibble::tibble(
        .name_repair = "minimal",
        !!name := if (is.list(value)) list(value) else value
      )
    })
  })

  unnamed_cols <- res |>
    dplyr::select(starts_with(unnamed)) |>
    colnames() |>
    rlang::syms()

  res |>
    dplyr::rowwise() |>
    dplyr::mutate(unnamed = list(rlang::list2(!!!unnamed_cols)), .keep = "unused") |>
    dplyr::ungroup()
}
