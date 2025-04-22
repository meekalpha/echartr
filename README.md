
# echartr

<!-- badges: start -->
<!-- badges: end -->

echartr provides an R interface to Apache Echarts, matching the Echarts Javascript API as closely as possible so that you can refer directly to https://echarts.apache.org/en/index.html.


## Working with `data.frames`

echartr provides helper functions for building echart options from `data.frames`

```{r}
library(echartr)

echartr(option = list(
  xAxis = list(type = "value"),
  yAxis = list(type = "value"),
  series = ec_scatter(
    iris,
    x = Sepal.Length,
    y = Sepal.Width,
    name = as.character(Species),
    symbol = dplyr::case_when(
      Species == "setosa" ~ "circle",
      Species == "versicolor" ~ "diamond",
      Species == "virginica" ~ "triangle"
    )
  )
))

```

## Installation

You can install the development version of echartr like so:

``` r
# install.packages("remotes")
remotes::install_github("meekalpha/echartr")
```
