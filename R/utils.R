#' Zip multiple lists/vectors together
#' e.g. turns 2 lists into a list of pairs or 3 lists into a list of tuples
zip <- function(...) {
  purrr::pmap(.l = rlang::list2(...), c)
}

#' Evaluate e once for each row in df
sub_values <- function(df, e) {
  e <- enexpr(e)
  map(zip(!!!df), ~{
    eval_tidy(e, env = list2env(as.list(.x)))
  })
}

#' Turn args into list of subbed vals
sub_args <- function(df, ...) {
  unname(imap(enexprs(...), ~sub_values(df, list2(!!.y := !!.x))))
}

#' Generate an expression that concatenate expressions
expr_c <- function(...) {
  expr(c(!!!enexprs(...)))
}
