test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

test_that("No attributes gives a single series", {
  data <- tibble::tibble(x = 1:50)
  expect_identical(
    ec_series(data, x),
    list(
      list(data = data$x)
    )
  )
})

test_that("Attributes with a single value give a single series", {
  data <- tibble::tibble(x = 1:50, name = "My series")
  expect_identical(
    ec_series(data, x, name = name),
    list(
      list(name = "My series", data = data$x)
    )
  )
})


test_that("Attributes with a multiple values give multiple series", {
  data <- tibble::tibble(x = 1:50, name = rep(c("Series 1", "Series 2"), 25))
  expect_identical(
    ec_series(data, x, name = name),
    list(
      list(name = "Series 1", data = seq(1L, 50L, 2L)),
      list(name = "Series 2", data = seq(2L, 50L, 2L))
    )
  )
})

test_that("Multiple attributes give a series for each combination", {
  data <- tibble::tibble(
    x = 1:60,
    name = rep(c("Series 1", "Series 2"), 30),
    type = rep(letters[1:4], 15)
  )

  # Ordering is left to right from args
  expect_identical(
    ec_series(data, x, name = name, type = type),
    list(
      list(name = "Series 1", type = "a", data = seq(1L, 60L, 4L)),
      list(name = "Series 1", type = "c", data = seq(3L, 60L, 4L)),
      list(name = "Series 2", type = "b", data = seq(2L, 60L, 4L)),
      list(name = "Series 2", type = "d", data = seq(4L, 60L, 4L))
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
    ec_series(data, x, name = name, label = list(fontSize = label_size)),
    list(
      list(name = "Series 1", label = list(fontSize = 11L), data = seq(1L, 60L, 4L)),
      list(name = "Series 1", label = list(fontSize = 13L),  data = seq(3L, 60L, 4L)),
      list(name = "Series 2", label = list(fontSize = 12L),  data = seq(2L, 60L, 4L)),
      list(name = "Series 2", label = list(fontSize = 14L),  data = seq(4L, 60L, 4L))
    )
  )
})

test_that("Warning on no data dimensions", {
  expect_warning(ec_series(
    tibble::tibble()
  ))
  expect_warning(ec_series(
    tibble::tibble(name = 1:5), name = name
  ))
})
