#' @export
new_option <- function(option = list()) {
  class(option) <- "ec_option"
  option
}

#' @export
ec_as_js <- function(option) {
  paste0(
    "option = ",
    jsonlite::toJSON(unclass(option), pretty = TRUE, auto_unbox = TRUE),
    ";"
  )
}

#' @export
print.ec_option <- function(x) {
  cat(ec_as_js(x))
  x
}


#' @export
ec_clip_js <- function(option) {
  rlang::check_installed("clipr")
  clipr::write_clip(ec_as_js(option))
}
