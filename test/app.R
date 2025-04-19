library(rlang)
library(purrr)
library(dplyr)
library(shiny)
library(echartr)
library(tidyverse)
library(rlang)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            sliderInput("size",
                        "symbolSize:",
                        min = 1,
                        max = 50,
                        value = 4),
            selectInput(
              inputId = "chart_type",
              label = "Chart type:",
              choices = c(
                "scatter", "line"
              )
            ),
            actionButton("button", "no more")
        ),

        # Show a plot of the generated distribution
        mainPanel(
          echartrOutput("plot")
        )
    )
)

# df <- iris |>
#   mutate(Species = as.character(Species)) |>
#   group_by(Species) |>
#   summarise(across(where(is.numeric), sum))

df <- iris |>
  arrange(Sepal.Width)

# Define server logic required to draw a histogram
server <- function(input, output) {

  # rv <- reactiveValues(option = new_option(
  #   xAxis = list(list(type = "category", data = df$Species), list(type = "category", data = df$Species)),
  #   yAxis = list(list(type = "value"), list(type = "value")),
  #   tooltip = list(show = TRUE),
  #   legend = list(show = TRUE),
  #   series = c(
  #     ec_series(
  #        df,
  #        type = "scatter",
  #        name = paste(Species, "sepals"),
  #        Species,
  #        Sepal.Width,
  #        symbolSize = 5,
  #        yAxisIndex = 0
  #     ),
  #     ec_series(df, type = "bar", name = paste(Species, "petals"), Species, Petal.Width, symbolSize = 5, yAxisIndex = 1)
  #   )
  # ))

  rv <- reactiveValues(
    symbol_size = 5
  )

  option <- reactive({
    new_option(
      xAxis = list(type = "value"),
      yAxis = list(type = "value"),
      series = ec_series(
        df, Sepal.Width, Sepal.Length,
        type = input$chart_type
      )
    )
  })

  output$plot <- renderEchartr({
    print("render")
    isolate(echartr(option = option()))
  })

  observe({
    print("update")
    updateEchartr(outputId = "plot", option = option())
  }) |>
    bindEvent(option(), ignoreInit = TRUE)



  # observe({
  #   isolate({
  #     symbolSize <- rv$symbolSize
  #     rv$symbolSize <- rv$symbolSize + 1
  #   })
  #
  #   rv$option$series <- c(
  #     ec_series(df, type = "scatter", name = paste(Species, "sepals"), Sepal.Width, Sepal.Length, symbolSize = !!symbolSize),
  #     ec_series(df, type = "scatter", name = paste(Species, "petals"), Petal.Width, Petal.Length, symbolSize = !!symbolSize)
  #   )
  # }) |>
  #   bindEvent(input$button)

  # observe({
  #   invalidateLater(5000L)
  #   isolate({
  #     rv$opts$series <- list(list(
  #       name = "test",
  #       type = "scatter",
  #       symbolSize = 4,
  #       data = as_seriec_data6(df, Petal.Length, Petal.Width)
  #     ))
  #   })
  #   updateEchartr(outputId = "plot", option = rv$opts)
  # })



  observeEvent(input$bins, {
    updateEchartr(outputId = "plot", option = NULL, on = list(
      list(eventName = "click", query = "series.line", handler = JS("x => console.log('click')")),
      list(eventName = "click", query = "series", handler = JS("x => console.log(this.context.)"))
    ))
  })
  #
  # observeEvent(input$button, {
  #   updateEchartr(outputId = "plot", option = NULL, off = list(
  #     list(eventName = "mousemove")
  #   ))
  # })
}

# Run the application
shinyApp(ui = ui, server = server)
