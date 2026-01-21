#' Create Panel fsQCA Parameters
#'
#' @description
#' Creates a parameter object for panel fsQCA analysis with default values
#' that can be customized.
#'
#' @param incl_cut Numeric. Inclusion (consistency) cutoff for truth table.
#'   Default is 0.70.
#' @param pri_cut Numeric. Proportional Reduction in Inconsistency cutoff.
#'   Default is 0.50.
#' @param n_cut Integer. Minimum number of cases for a truth table row.
#'   Default is 5.
#' @param nec_threshold Numeric. Consistency threshold for necessity.
#'   Default is 0.90.
#' @param robustness_B Integer. Number of iterations for robustness testing.
#'   Default is 999.
#' @param robustness_drop Numeric. Proportion of cases to drop per iteration.
#'   Default is 0.10.
#' @param robustness_seed Integer. Random seed for reproducibility.
#'   Default is 123.
#' @param robust_PI_freq_cut Numeric. Minimum frequency for robust prime implicants.
#'   Default is 0.80.
#' @param robust_nec_cut Numeric. Minimum share for robust necessity.
#'   Default is 0.80.
#'
#' @return A list of class \code{pfsqca_params} containing all parameters.
#'
#' @examples
#' # Default parameters
#' params <- pfsqca_params()
#'
#' # Custom parameters
#' params <- pfsqca_params(incl_cut = 0.75, robustness_B = 500)
#'
#' @export
pfsqca_params <- function(
    incl_cut = 0.70,
    pri_cut = 0.50,
    n_cut = 5L,
    nec_threshold = 0.90,
    robustness_B = 999L,
    robustness_drop = 0.10,
    robustness_seed = 123L,
    robust_PI_freq_cut = 0.80,
    robust_nec_cut = 0.80
) {

  params <- list(
    incl_cut = incl_cut,
    pri_cut = pri_cut,
    n_cut = as.integer(n_cut),
    nec_threshold = nec_threshold,
    robustness_B = as.integer(robustness_B),
    robustness_drop = robustness_drop,
    robustness_seed = as.integer(robustness_seed),
    robust_PI_freq_cut = robust_PI_freq_cut,
    robust_nec_cut = robust_nec_cut
  )

  class(params) <- "pfsqca_params"
  params
}


#' Print method for pfsqca_params
#'
#' @param x A pfsqca_params object.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns x.
#'
#' @export
print.pfsqca_params <- function(x, ...) {
  cat("Panel fsQCA Parameters\n")
  cat("======================\n\n")

  cat("Sufficiency thresholds:\n")
  cat("  incl_cut:", x$incl_cut, "\n")
  cat("  pri_cut:", x$pri_cut, "\n")
  cat("  n_cut:", x$n_cut, "\n")

  cat("\nNecessity threshold:\n")
  cat("  nec_threshold:", x$nec_threshold, "\n")

  cat("\nRobustness testing:\n")
  cat("  robustness_B:", x$robustness_B, "iterations\n")
  cat("  robustness_drop:", x$robustness_drop * 100, "%\n")
  cat("  robustness_seed:", x$robustness_seed, "\n")
  cat("  robust_PI_freq_cut:", x$robust_PI_freq_cut, "\n")
  cat("  robust_nec_cut:", x$robust_nec_cut, "\n")

  invisible(x)
}
