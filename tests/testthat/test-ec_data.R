
test_that("One dimensional data gives vector", {
  data <- tibble::tibble(x = 1:50)
  expect_identical(
    ec_data(data, x), data$x
  )
})

test_that("Multi dimensional numeric data gives list of vectors", {
  data <- tibble::tibble(x = 1:5, y = 6:10, z = 11:15)
  expect_identical(
    ec_data(data, x, y, z),
    list(
      c(1L, 6L, 11L),
      c(2L, 7L, 12L),
      c(3L, 8L, 13L),
      c(4L, 9L, 14L),
      c(5L, 10L, 15L)
    )
  )
})

test_that("Multi-type data gives list of lists", {
  data <- tibble::tibble(x = letters[1:5], y = 6:10)
  expect_identical(
    ec_data(data, x, y),
    list(
      list("a", 6L),
      list("b", 7L),
      list("c", 8L),
      list("d", 9L),
      list("e", 10L)
    )
  )
})

test_that("Data can have value and name", {
  data <- tibble::tibble(x = 1:5, y = 6:10, z = 11:15, name = letters[1:5])
  expect_identical(
    ec_data(data, x, y, z, name = name),
    list(
      list(name = "a", value = c(1L, 6L, 11L)),
      list(name = "b", value = c(2L, 7L, 12L)),
      list(name = "c", value = c(3L, 8L, 13L)),
      list(name = "d", value = c(4L, 9L, 14L)),
      list(name = "e", value = c(5L, 10L, 15L))
    )
  )
})

test_that("Data can have name", {
  data <- tibble::tibble(x = 1:5, y = 6:10, z = 11:15, name = letters[1:5])
  expect_identical(
    ec_data(data, x, y, z, name = name),
    list(
      list(name = "a", value = c(1L, 6L, 11L)),
      list(name = "b", value = c(2L, 7L, 12L)),
      list(name = "c", value = c(3L, 8L, 13L)),
      list(name = "d", value = c(4L, 9L, 14L)),
      list(name = "e", value = c(5L, 10L, 15L))
    )
  )
})

test_that("Data can have styling", {
  data <- tibble::tibble(
    x = 1:6, y = 7:12, color = rep(c("blue", "green"), 3)
  )
  expect_identical(
    ec_data(data, x, y, itemStyle = list(color = color)),
    list(
      list(itemStyle = list(color = "blue"), value = c(1L, 7L)),
      list(itemStyle = list(color = "green"), value = c(2L, 8L)),
      list(itemStyle = list(color = "blue"), value = c(3L, 9L)),
      list(itemStyle = list(color = "green"), value = c(4L, 10L)),
      list(itemStyle = list(color = "blue"), value = c(5L, 11L)),
      list(itemStyle = list(color = "green"), value = c(6L, 12L))
    )
  )
})
