test_that("unique_coverage returns correct structure", {
  data <- data.frame(
    outcome = c(0.2, 0.5, 0.8, 0.9),
    A = c(0.3, 0.6, 0.7, 0.85),
    B = c(0.4, 0.5, 0.6, 0.9)
  )

  result <- unique_coverage(data, "outcome", c("A", "B"))

  expect_true("term" %in% names(result))
  expect_true("raw_cov" %in% names(result))
  expect_true("unique_cov" %in% names(result))
  expect_true("cons" %in% names(result))
  expect_equal(nrow(result), 2)
})

test_that("unique_coverage handles single term", {
  data <- data.frame(
    outcome = c(0.2, 0.5, 0.8, 0.9),
    A = c(0.3, 0.6, 0.7, 0.85)
  )

  result <- unique_coverage(data, "outcome", "A")

  expect_equal(unname(result$raw_cov), unname(result$unique_cov))
})

test_that("unique_coverage handles empty terms", {
  data <- data.frame(outcome = c(0.2, 0.5, 0.8))

  result <- unique_coverage(data, "outcome", character(0))

  expect_equal(nrow(result), 0)
})

test_that("unique_cov is less than or equal to raw_cov", {
  data <- data.frame(
    outcome = c(0.2, 0.5, 0.8, 0.9),
    A = c(0.3, 0.6, 0.7, 0.85),
    B = c(0.4, 0.5, 0.6, 0.9)
  )

  result <- unique_coverage(data, "outcome", c("A", "B"))

  expect_true(all(result$unique_cov <= result$raw_cov))
})
