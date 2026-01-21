test_that("literal_membership handles presence", {
  data <- data.frame(income = c(0.2, 0.5, 0.8))

  result <- literal_membership(data, "income")

  expect_equal(result, c(0.2, 0.5, 0.8))
})

test_that("literal_membership handles negation", {
  data <- data.frame(income = c(0.2, 0.5, 0.8))

  result <- literal_membership(data, "~income")

  expect_equal(result, c(0.8, 0.5, 0.2))
})

test_that("term_membership calculates conjunction", {
  data <- data.frame(
    A = c(0.2, 0.6, 0.9),
    B = c(0.4, 0.3, 0.8)
  )

  result <- term_membership(data, "A*B")

  expect_equal(result, pmin(data$A, data$B))
})

test_that("term_membership handles negation in terms", {
  data <- data.frame(
    A = c(0.2, 0.6, 0.9),
    B = c(0.4, 0.3, 0.8)
  )

  result <- term_membership(data, "A*~B")

  expect_equal(result, pmin(data$A, 1 - data$B))
})

test_that("solution_membership calculates disjunction", {
  data <- data.frame(
    A = c(0.2, 0.6, 0.9),
    B = c(0.4, 0.3, 0.8)
  )

  result <- solution_membership(data, c("A", "B"))

  expect_equal(result, pmax(data$A, data$B))
})

test_that("solution_membership handles empty terms", {
  data <- data.frame(A = c(0.2, 0.6, 0.9))

  result <- solution_membership(data, character(0))

  expect_true(all(is.na(result)))
})
