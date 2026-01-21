test_that("calibrate_percentile returns values between 0 and 1", {
  x <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)

  result <- calibrate_percentile(x)

  expect_true(all(result >= 0 & result <= 1))
  expect_length(result, length(x))
})

test_that("calibrate_percentile handles custom percentiles", {
  x <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)

  result <- calibrate_percentile(x, probs = c(0.05, 0.50, 0.95))

  expect_true(all(result >= 0 & result <= 1))
})

test_that("calibrate_panel calibrates multiple variables", {
  data <- data.frame(
    var1 = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100),
    var2 = c(5, 15, 25, 35, 45, 55, 65, 75, 85, 95)
  )

  result <- calibrate_panel(data, vars = c("var1", "var2"))

  expect_true(all(result$var1 >= 0 & result$var1 <= 1))
  expect_true(all(result$var2 >= 0 & result$var2 <= 1))
})

test_that("calibrate_percentile handles constant values", {
  x <- rep(50, 10)

  result <- calibrate_percentile(x)

  expect_true(all(is.na(result)))
})
