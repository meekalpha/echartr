
# 1. Dimensions (x, y, ...) are provided via unnamed variables
# 2. Named variables used to
# 3. Both unnamed + named variables are evaluated


# Scatter chart
echartr(option = list(
  xAxis = list(type = "value"),
  yAxis = list(type = "value"),
  series = ec_scatter(iris,
    Sepal.Width, Sepal.Length,
    name = as.character(Species)
  )
))

echartr(option = list(
  xAxis = list(type = "value"),
  yAxis = list(type = "value"),
  series = ec_scatter(iris,
    Sepal.Width, Sepal.Length,
    symbol = case_when(Species == "setosa" ~ "diamond", TRUE ~ "circle"),
    name = as.character(Species)
  )
))

new_option(
  xAxis = list(type = "value"),
  yAxis = list(type = "value"),
  series = ec_scatter(iris,
                      Sepal.Width, Sepal.Length,
                      name = as.character(Species)
  )
)

# Line
x <- c(1:99)
random_y <- rnorm(99, mean = 0)
data <- data.frame(x, random_y)

echartr(option = list(
  xAxis = list(type = "value"),
  yAxis = list(type = "value", min = -5, max = 5),
  series = ec_line(data,
    x = x, y = random_y
  )
))


# Area


#

echartr(option = list(
  xAxis = list(type = "value"),
  yAxis = list(type = "value"),
  legend = list(show = TRUE),
  series = ec_series(
    iris,
    data = list(value = c(Petal.Width, Petal.Length)),
    type = "scatter",
    color = dplyr::case_when(
      Species == "setosa" ~ "red",
      Species == "virginica" ~ "green",
      TRUE ~ "yellow"
    ),
    name = as.character(Species),
    label = list(show = (Species == "setosa"), color = "black", verticalAlign = "top")
  )
))


echartr(option = list(
  xAxis = list(type = "value"),
  yAxis = list(type = "value"),
  legend = list(show = TRUE),
  series = ec_series(
    iris,
    data = list(value = c(Petal.Width, Petal.Length)),
    type = "scatter",
    color = dplyr::case_when(
      Species == "setosa" ~ "red",
      Species == "virginica" ~ "green",
      TRUE ~ "yellow"
    ),
    name = as.character(Species),
    label = list(show = (Species == "setosa"), color = "black", verticalAlign = "top")
  )
))


# Timeline -----------------------

df <- tibble::as_tibble(iris) |>
  dplyr::mutate(across(Species, as.character))

option <- new_option(
  baseOption = list(
    xAxis = list(type = "value", min = 0, max = 3),
    yAxis = list(type = "value", min = 0, max = 7)
  ),
  timeline = list(
    axisType = "category",
    data = unique(df$Species)
  ),
  options = ec_options(
    df, Sepal.Length, Sepal.Width, name = Species,
    series = list(type = "scatter", name = Species)
  )
)

ec_options(
  df, Sepal.Length, Sepal.Width,
  #name = Species,
  series = list(type = "scatter")
)

ec_clip_js(option)

echartr(option)

#-------------------------
# Stacked bar

dates <- seq(as.Date("2020-01-01"), as.Date("2020-12-01"), by = "months")
groups <- LETTERS[1:5]
df <- tidyr::crossing(dates, groups)
df$value <- runif(length(dates) * length(groups), 0, 100)

echartr(option = list(
  xAxis = list(type = "category"), # Category works better than date for stacked bar charts
  yAxis = list(type = "value"),
  legend = list(show = TRUE),
  series = ec_series(
    df, dates, value,
    type = "bar",
    stack = "stack",
    name = groups
  )
))

#-----------------------------
# Timelined bar

dates <- seq(as.Date("2020-01-01"), as.Date("2020-12-01"), by = "months")
groups <- LETTERS[1:5]
df <- tidyr::crossing(dates, groups)
df$value <- runif(length(dates) * length(groups), 0, 100)

echartr(option = list(
  baseOption = list(
    xAxis = list(type = "category"), # Category works better than date for stacked bar charts
    yAxis = list(type = "value"),
    legend = list(show = TRUE)
  ),
  timeline = list(
    axisType = "category",
    data = LETTERS[1:5]
  ),
  options = ec_options(
    df, dates, value, name = groups,
    series = list(type = "bar", name = groups)
  )
))

years <- c("2001", "2002", "2003", "2004")
group1 <- LETTERS[1:5]
group2 <- letters[1:5]
df <- tidyr::crossing(years, group1, group2)
df$value <- runif(length(years) * length(group1) * length(group2), 0, 100)

echartr(option = list(
  baseOption = list(
    xAxis = list(type = "category"), # Category works better than date for stacked bar charts
    yAxis = list(type = "value"),
    legend = list(show = TRUE)
  ),
  timeline = list(
    axisType = "category",
    data = years
  ),
  options = ec_options(
    df, group1, value, name = years, stack = "stack",
    series = list(type = "bar", name = group2)
  )
))


# -----------------------------------------
# Grid
echartr(option = list(
  xAxis = list(
    list(type = "value", gridIndex = 0),
    list(type = "value", gridIndex = 1)
  ),
  yAxis = list(
    list(type = "value", gridIndex = 0),
    list(type = "value", gridIndex = 1)
  ),
  grid = list(
    list(top = "10%", height = "30%"),
    list(top = "60%", height = "30%")
  ),
  title = list(
    list(text = "Petals"),
    list(text = "Sepals", top = "50%")
  ),
  legend = list(show = TRUE),
  series = c(
    ec_series(iris,
      Petal.Width, Petal.Length,
      name = as.character(Species),
      xAxisIndex = 0, yAxisIndex = 0,
      type = "scatter",
    ),
    ec_series(iris,
      Sepal.Width, Sepal.Length,
      name = as.character(Species),
      xAxisIndex = 1, yAxisIndex = 1,
      type = "scatter",
    )
  )
))

