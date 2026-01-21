test_that("pfsqca_params creates valid object", {
  params <- pfsqca_params()

  expect_s3_class(params, "pfsqca_params")
  expect_equal(params$incl_cut, 0.70)
  expect_equal(params$pri_cut, 0.50)
  expect_equal(params$n_cut, 5L)
  expect_equal(params$nec_threshold, 0.90)
  expect_equal(params$robustness_B, 999L)
})

test_that("pfsqca_params accepts custom values", {
  params <- pfsqca_params(incl_cut = 0.80, robustness_B = 100)

  expect_equal(params$incl_cut, 0.80)
  expect_equal(params$robustness_B, 100L)
})

test_that("print.pfsqca_params works", {
  params <- pfsqca_params()

  expect_output(print(params), "Panel fsQCA Parameters")
  expect_output(print(params), "incl_cut")
})
