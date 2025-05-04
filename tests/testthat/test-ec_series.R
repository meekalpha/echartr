
test_that("No attributes gives a single series", {
  data <- tibble::tibble(x = 1:50)
  expect_identical(
    unclass(ec_series(data, x, type = "bar")),
    list(
      list(type = "bar", data = data$x)
    )
  )
})

test_that("Attributes with a single value give a single series", {
  data <- tibble::tibble(x = 1:50, name = "My series")
  expect_identical(
    unclass(ec_series(data, x, name = name, type = "bar")),
    list(
      list(name = "My series", type = "bar", data = data$x)
    )
  )
})

test_that("Attributes with a multiple values give multiple series", {
  data <- tibble::tibble(x = 1:50, name = rep(c("Series 1", "Series 2"), 25))
  expect_identical(
    unclass(ec_series(data, x, name = name, type = "bar")),
    list(
      list(name = "Series 1", type = "bar", data = seq(1L, 50L, 2L)),
      list(name = "Series 2", type = "bar", data = seq(2L, 50L, 2L))
    )
  )
})

test_that("Multiple attributes give a series for each combination", {
  data <- tibble::tibble(
    x = 1:60,
    name = rep(c("Series 1", "Series 2"), 30),
    type = rep(c("bar", "scatter", "pie", "line"), 15)
  )

  # Ordering is left to right from args
  expect_identical(
    unclass(ec_series(data, x, name = name, type = type)),
    list(
      list(name = "Series 1", type = "bar", data = seq(1L, 60L, 4L)),
      list(name = "Series 2", type = "scatter", data = seq(2L, 60L, 4L)),
      list(name = "Series 1", type = "pie", data = seq(3L, 60L, 4L)),
      list(name = "Series 2", type = "line", data = seq(4L, 60L, 4L))
    )
  )
})

test_that("Series can be split on an styling", {
  data <- tibble::tibble(
    x = 1:60,
    name = rep(c("Series 1", "Series 2"), 30),
    label_size = rep(11:14, 15)
  )

  # Ordering is left to right from args
  expect_identical(
    unclass(ec_series(
      data, x, name = name, label = list(fontSize = label_size), type = "bar"
    )),
    list(
      list(name = "Series 1", label = list(fontSize = 11L), type = "bar", data = seq(1L, 60L, 4L)),
      list(name = "Series 2", label = list(fontSize = 12L), type = "bar", data = seq(2L, 60L, 4L)),
      list(name = "Series 1", label = list(fontSize = 13L), type = "bar", data = seq(3L, 60L, 4L)),
      list(name = "Series 2", label = list(fontSize = 14L), type = "bar", data = seq(4L, 60L, 4L))
    )
  )
})

test_that("Warning on no data dimensions", {
  expect_warning(ec_series(
    tibble::tibble(), type = "bar"
  ))
  expect_warning(ec_series(
    tibble::tibble(name = 1:5), name = name, type = "bar"
  ))
})
