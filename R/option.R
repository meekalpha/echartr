#' Create a new echartr 'option' object
#' @param ... Name-value pairs, representing echart option attributes
#' @export
new_option <- function(...) {
  structure(list(...), class = c("ec_option", "ec_object"))
}

