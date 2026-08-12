test_that("check_columns names the missing column and lists what exists", {
  data <- data.frame(case_id = 1, period = 1, income = 0.5)

  expect_error(check_columns(data, "incom", "condition"), "incom")
  expect_error(check_columns(data, "incom", "condition"), "case_id")
  expect_silent(check_columns(data, c("income", "period")))
})

test_that("check_calibrated flags uncalibrated variables", {
  raw <- data.frame(x = c(10, 50, 90))
  cal <- data.frame(x = c(0.1, 0.5, 0.9))

  expect_warning(check_calibrated(raw, "x"), "not fuzzy-set scores")
  expect_error(check_calibrated(raw, "x", action = "error"), "not fuzzy-set scores")
  expect_silent(check_calibrated(cal, "x"))
})

test_that("analysis functions refuse a misspelled column", {
  data_cal <- calibrate_panel(example_panel, vars = c("infrastructure", "entrepreneurship"))

  expect_error(
    necessity_test(data_cal, "entrepreneurship", "infrastructur"),
    "infrastructur"
  )
  expect_error(
    panel_metrics(data_cal, "entrepreneurship", "infrastructure", time_var = "year"),
    "year"
  )
})

test_that("calibrate_panel warns when a variable has no variation", {
  data <- data.frame(flat = rep(7, 10), varies = 1:10)

  expect_warning(out <- calibrate_panel(data, vars = c("flat", "varies")), "no variation")
  expect_true(all(is.na(out$flat)))
  expect_false(any(is.na(out$varies)))
})
