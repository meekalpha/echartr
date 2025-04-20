#' Create a new echartr 'option' object
#' @export
new_option <- function(...) {
  structure(list(...), class = c("ec_option", "ec_object"))
}

