test_that("necessity_test returns correct structure", {
  data <- data.frame(
    outcome = c(0.2, 0.5, 0.8, 0.9, 0.3),
    cond1 = c(0.3, 0.6, 0.7, 0.95, 0.4)
  )

  result <- necessity_test(data, "outcome", "cond1")

  expect_true("condition" %in% names(result))
  expect_true("inclN" %in% names(result))
  expect_true("covN" %in% names(result))
  expect_true("nec_flag" %in% names(result))
  expect_equal(nrow(result), 2)  # cond1 and ~cond1
})

test_that("necessity_test uses custom params", {
  data <- data.frame(
    outcome = c(0.2, 0.5, 0.8, 0.9, 0.3),
    cond1 = c(0.9, 0.95, 0.85, 0.92, 0.88)
  )

  params <- pfsqca_params(nec_threshold = 0.80)
  result <- necessity_test(data, "outcome", "cond1", params)

  expect_true(any(result$nec_flag))
})

test_that("necessity_test orders by inclN descending", {
  data <- data.frame(
    outcome = c(0.2, 0.5, 0.8, 0.9, 0.3),
    cond1 = c(0.3, 0.6, 0.7, 0.95, 0.4)
  )

  result <- necessity_test(data, "outcome", "cond1")

  expect_true(result$inclN[1] >= result$inclN[2])
})
