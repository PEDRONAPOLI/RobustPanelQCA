conditions <- c("infrastructure", "knowledge", "finance", "talent")

test_that("run_pfsqca recovers the two paths built into the example data", {
  res <- run_pfsqca(
    example_panel, "entrepreneurship", conditions,
    params = pfsqca_params(incl_cut = 0.80, n_cut = 1),
    robustness = FALSE, verbose = FALSE
  )

  expect_s3_class(res, "pfsqca_result")
  expect_setequal(res$terms, c("infrastructure*knowledge", "finance*talent"))

  # No condition is necessary: either path alone produces the outcome.
  expect_false(any(res$necessity$nec_flag))

  pooled <- res$panel_metrics$pooled
  expect_gt(as.numeric(pooled$POCONS[pooled$component == "solution"]), 0.90)
})

test_that("run_pfsqca validates its inputs", {
  expect_error(
    run_pfsqca(example_panel, "entrepreneurship", "infrastructure", verbose = FALSE),
    "At least two conditions"
  )
  expect_error(
    run_pfsqca(example_panel, "entrepreneurship",
               c("entrepreneurship", "knowledge"), verbose = FALSE),
    "also listed as a condition"
  )
  expect_error(
    run_pfsqca(example_panel, "entrepreneurship", conditions,
               time_var = "year", verbose = FALSE),
    "year"
  )
  expect_error(
    run_pfsqca(example_panel, "entrepreneurship", conditions,
               calibrate = FALSE, verbose = FALSE),
    "not fuzzy-set scores"
  )
})

test_that("sufficiency_analysis explains impossible thresholds", {
  data_cal <- calibrate_panel(example_panel, vars = c(conditions, "entrepreneurship"))

  expect_error(
    sufficiency_analysis(data_cal, "entrepreneurship", conditions,
                         pfsqca_params(incl_cut = 0.999, n_cut = 50)),
    "incl_cut"
  )
})

test_that("panel metrics keep the grouping column names the user supplied", {
  data_cal <- calibrate_panel(example_panel, vars = c(conditions, "entrepreneurship"))
  pm <- panel_metrics(data_cal, "entrepreneurship", "infrastructure*knowledge")

  expect_true("period" %in% names(pm$between))
  expect_true("case_id" %in% names(pm$within))

  nd <- necessity_panel_diagnostics(data_cal, "entrepreneurship", conditions)
  expect_true("period" %in% names(nd$between))
  expect_true("case_id" %in% names(nd$within))
})

test_that("export_pfsqca writes the tables and the parameter record", {
  res <- run_pfsqca(
    example_panel, "entrepreneurship", conditions,
    params = pfsqca_params(incl_cut = 0.80, n_cut = 1),
    robustness = FALSE, verbose = FALSE
  )
  out <- file.path(tempdir(), "export_test")
  unlink(out, recursive = TRUE)

  files <- export_pfsqca(res, dir = out, quiet = TRUE)

  expect_true(length(files) > 5)
  expect_true(file.exists(file.path(out, "00_parameters.txt")))
  expect_true(file.exists(file.path(out, "01_necessity.csv")))
  expect_true(file.exists(file.path(out, "05_solution_paths.csv")))

  record <- readLines(file.path(out, "00_parameters.txt"))
  expect_true(any(grepl("entrepreneurship", record)))

  unlink(out, recursive = TRUE)
})

test_that("use_pfsqca_template copies the script and the example data", {
  out <- file.path(tempdir(), "template_test")
  unlink(out, recursive = TRUE)

  files <- use_pfsqca_template(path = out)

  expect_true(file.exists(file.path(out, "run_analysis.R")))
  expect_true(file.exists(file.path(out, "example_panel.csv")))
  expect_length(files, 2)

  # Existing files are preserved unless overwrite = TRUE
  expect_message(use_pfsqca_template(path = out), "already exists")

  unlink(out, recursive = TRUE)
})

test_that("print.pfsqca_result reports paths and panel stability", {
  res <- run_pfsqca(
    example_panel, "entrepreneurship", conditions,
    params = pfsqca_params(incl_cut = 0.80, n_cut = 1),
    robustness = FALSE, verbose = FALSE
  )

  expect_output(print(res), "SUFFICIENT PATHS")
  expect_output(print(res), "POCONS")
  expect_output(print(res), "PANEL STABILITY")
  expect_output(print(res), "infrastructure\\*knowledge")
})
