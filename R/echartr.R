#' Create an Echart
#'
#' @param option list-tree representation of echarts option argument, see [https://echarts.apache.org/en/option.html]
#' @param on list of event listeners to register, see [https://echarts.apache.org/en/api.html#echartsInstance.on]
#' @param listen character vector of events to include in Shiny input, any of [https://echarts.apache.org/en/api.html#events]
#'
#'   Event output is available as `input$<outputId>_<event>`. For mouse events, event data is limited.
#'
#' @section Events:
#'
#' [https://echarts.apache.org/en/api.html#events]
#'
#' Event listeners can be registered via the `on` argument.
#' Each listener is a named list, for example:
#'
#' ```r
#' echartr(
#'  option = ...,
#'  on = list(
#'    list(
#'      eventName = "click",
#'      handler = htmlwidgets::JS("params => console.log(params)")
#'    ),
#'    list(
#'      eventName = "selectchanged",
#'      query = ,
#'      handler = htmlwidgets::JS("function(params) {
#'        console.log(x);
#'      }")
#'    )
#'  )
#' )
#' ```
#' The `context` for the handler is the Echart instance - this can be accessed within the handler via `this.context`
#'
#' @import htmlwidgets
#'
#' @export
echartr <- function(
  option = list(),
  on = NULL,
  listen = character(),
  elementId = NULL
) {

  if (!purrr::is_empty(listen) && !shiny::isRunning()) {
    warning("`listen` argument is only available in Shiny applications")
  }

  # forward options using x
  x = list(
    option = option,
    on = on,
    off = NULL,
    listen = as.list(listen)
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'echartr',
    x,
    package = 'echartr',
    elementId = elementId
  )
}

#' Shiny bindings for echartr
#'
#' Output and render functions for using echartr within Shiny
#' applications and interactive Rmd documents.
#'
#' @param outputId output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a echartr
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#'
#' @name echartr-shiny
#'
#' @export
echartrOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'echartr', width, height, package = 'echartr')
}

#' @rdname echartr-shiny
#' @export
renderEchartr <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, echartrOutput, env, quoted = TRUE)
}

#' Update an echartr instance
#'
#' @description
#' `updateEchartr()` updates an echartr instance within a Shiny application.
#'
#' @param session The Shiny session object. Defaults to the current Shiny session.
#' @param outputId The Shiny output ID of the `echartr` instance.
#' @param option list-tree representation of echart option argument, see https://echarts.apache.org/en/option.html
#' @param on list of event listeners to register, see https://echarts.apache.org/en/api.html#echartsInstance.on
#' @param off list of event listeners to de-register, see https://echarts.apache.org/en/api.html#echartsInstance.off
#'
#' Intention is to support all functions under https://echarts.apache.org/en/api.html#echartsInstance
#' @export
updateEchartr <- function(
  session = shiny::getDefaultReactiveDomain(),
  outputId,
  option = NULL,
  on = NULL,
  off = NULL,
  listen = character()
) {
  session$sendCustomMessage(
    sprintf("__echartr__%s", outputId),
    list(option = option, on = on, off = off)
  )
}


