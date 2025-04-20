#' Convert an echartr list-tree object into javascript
#'
#' @param x echartr list-tree
#' @export
ec_as_js <- function(x) {
  UseMethod("ec_as_js")
}

#' Copy the javascript equivalent of an echartr list-tree object to the clipboard
#'
#' @param x echartr list-tree
#' @export
ec_clip <- function(x) {
  UseMethod("ec_clip")
}


#' TODO: Handle Javascript better than this
#' @inheritParams ec_as_js
#' @exportS3Method
ec_as_js.ec_object <- function(x) {
  prepped <- x |>
    purrr::modify_tree(
      leaf = function(x) {
        if ("JS_EVAL" %in% class(x)) {
          paste0("!!JS_EVAL!!", unclass(x), "!!JS_EVAL!!")
        } else {
          unclass(x)
        }
      },
      is_node = is.list
    )

  jsonlite::toJSON(
    unclass(prepped),
    pretty = TRUE,
    auto_unbox = TRUE
  ) |>
    stringr::str_remove_all("\"!!JS_EVAL!!") |>
    stringr::str_remove_all("!!JS_EVAL!!\"")
}

#' @exportS3Method
ec_as_js.ec_option <- function(x) {
  paste0("option = ", ec_as_js.ec_object(x), ";")
}

#' Copy the javascript representation of chart options
#' @inheritParams ec_clip
#' @export
ec_clip.ec_object <- function(x) {
  rlang::check_installed("clipr")
  clipr::write_clip(ec_as_js(x))
}

#' Print the javascript representation of an echartr list-tree object
#' @exportS3Method
print.ec_object <- function(x) {
  cat(ec_as_js(x))
  x
}

#' Print the javascript representation of an echartr list-tree object
#' @export
print.ec_option <- function(x) {
  cat(paste("option = ", ec_as_js(x), ";"))
  x
}
