#' Create Strict Anchors for Calibration
#'
#' @description
#' Internal function that handles tied quantile values by creating
#' strictly ordered anchors for calibration.
#'
#' @param q10 Numeric. The 10th percentile value.
#' @param q50 Numeric. The 50th percentile value (median).
#' @param q90 Numeric. The 90th percentile value.
#'
#' @return A numeric vector of length 3 with strictly ordered anchors,
#'   or NULL if valid anchors cannot be created.
#'
#' @keywords internal
make_strict_anchors <- function(q10, q50, q90) {
  if (isTRUE(all.equal(q10, q50)) && isTRUE(all.equal(q50, q90))) return(NULL)
  if (isTRUE(all.equal(q10, q50)) && (q90 > q50)) return(c(q10, (q50 + q90) / 2, q90))
  if (isTRUE(all.equal(q50, q90)) && (q50 > q10)) return(c(q10, (q10 + q50) / 2, q90))
  if (q10 < q50 && q50 < q90) return(c(q10, q50, q90))

  r <- q90 - q10
  eps <- max(1e-12, abs(r) * 1e-6)
  q10a <- q10
  q50a <- max(q50, q10a + eps)
  q90a <- max(q90, q50a + eps)
  if (!(q10a < q50a && q50a < q90a)) return(NULL)
  c(q10a, q50a, q90a)
}


#' Calibrate a Single Variable Using Percentiles
#'
#' @description
#' Calibrates a numeric vector to fuzzy-set membership scores using
#' percentile-based thresholds (10th, 50th, 90th by default).
#'
#' @param x Numeric vector to calibrate.
#' @param probs Numeric vector of length 3 with percentiles for
#'   full exclusion, crossover, and full inclusion. Default is c(0.10, 0.50, 0.90).
#'
#' @return Numeric vector of fuzzy-set membership scores between 0 and 1.
#'
#' @details
#' Uses the direct method of calibration with the QCA package.
#' Handles tied quantile values automatically.
#'
#' @examples
#' x <- c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
#' calibrate_percentile(x)
#'
#' @export
calibrate_percentile <- function(x, probs = c(0.10, 0.50, 0.90)) {
  x <- as.numeric(x)
  qs <- stats::quantile(x, probs = probs, na.rm = TRUE, names = FALSE)
  if (any(!is.finite(qs))) return(rep(NA_real_, length(x)))
  anchors <- make_strict_anchors(qs[1], qs[2], qs[3])
  if (is.null(anchors)) return(rep(NA_real_, length(x)))
  QCA::calibrate(x, type = "fuzzy", thresholds = anchors)
}


#' Calibrate Multiple Variables in a Data Frame
#'
#' @description
#' Calibrates multiple variables in a data frame to fuzzy-set membership
#' scores using percentile-based thresholds.
#'
#' @param data A data frame containing the variables to calibrate.
#' @param vars Character vector with names of variables to calibrate.
#' @param probs Numeric vector of length 3 with percentiles for
#'   full exclusion, crossover, and full inclusion. Default is c(0.10, 0.50, 0.90).
#'
#' @return A data frame with calibrated variables (original values replaced).
#'
#' @examples
#' \dontrun{
#' # Calibrate conditions and outcome
#' data_cal <- calibrate_panel(data, c("income", "patents", "outcome"))
#' }
#'
#' @export
calibrate_panel <- function(data, vars, probs = c(0.10, 0.50, 0.90)) {
  data_out <- data
  for (v in vars) {
    data_out[[v]] <- calibrate_percentile(data_out[[v]], probs = probs)
  }
  data_out
}
