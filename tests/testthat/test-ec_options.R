
test_that("No attributes gives a single option", {
  data <- tibble::tibble(x = 1:50)
  expect_identical(
    unclass(ec_options(data, x)),
    list(list(
      series = list(
        list(data = data$x)
      )
    ))
  )
})

test_that("Attributes with a single value give a single option", {
  data <- tibble::tibble(x = 1:50, name = "My option")
  expect_identical(
    unclass(ec_options(data, x, name = name)),
    list(
      list(name = "My option", series = list(list(data = data$x)))
    )
  )
})

test_that("Attributes with a multiple values give multiple series", {
  data <- tibble::tibble(x = 1:50, name = rep(c("Option 1", "Option 2"), 25))
  expect_identical(
    unclass(ec_options(data, x, name = name)),
    list(
      list(name = "Option 1", series = list(list(data = seq(1L, 50L, 2L)))),
      list(name = "Option 2", series = list(list(data = seq(2L, 50L, 2L))))
    )
  )
})

test_that("Can split each option into multiple series", {
  data <- tibble::tibble(
    x = 1:60,
    option_name = rep(c("Option 1", "Option 2"), 30),
    series_name = rep(c("Series 1", "Series 2", "Series 3", "Series 4"), 15)
  )
  expect_identical(
    unclass(ec_options(data, x, name = option_name, series = list(name = series_name))),
    list(
      list(name = "Option 1", series = list(
        list(name = "Series 1", data = seq(1L, 60L, 4L)),
        list(name = "Series 3", data = seq(3L, 60L, 4L))
      )),
      list(name = "Option 2", series = list(
        list(name = "Series 2", data = seq(2L, 60L, 4L)),
        list(name = "Series 4", data = seq(4L, 60L, 4L))
      ))
    )
  )
})

