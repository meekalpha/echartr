#' Create a new echartr 'option' object
#' @export
new_option <- function(...) {
  structure(list(...), class = "ec_option")
}

leaf_fn <- function(x) {
  if ("JS_EVAL" %in% class(x)) {
    paste0("!!JS_EVAL!!", unclass(x), "!!JS_EVAL!!")
  } else {
    unclass(x)
  }
}

#' Generate JS version
#' TODO: Handle Javascript properly so it's not a string in the final result
#' @export
ec_as_js <- function(option) {

  prepped <- option |>
    purrr::modify_tree(
      leaf = leaf_fn,
      is_node = is.list
    )

  paste0(
    "option = ",
    jsonlite::toJSON(
      unclass(prepped),
      pretty = TRUE,
      auto_unbox = TRUE
    ),
    ";"
  ) |>
    stringr::str_remove_all("\"!!JS_EVAL!!") |>
    stringr::str_remove_all("!!JS_EVAL!!\"")
}

#' Print the javascript representation of chart options
#' @export
print.ec_option <- function(x) {
  cat(ec_as_js(x))
  x
}


#' Copy the javascript representation of chart options
#' @export
ec_clip_js <- function(option) {
  rlang::check_installed("clipr")
  clipr::write_clip(ec_as_js(option))
}

