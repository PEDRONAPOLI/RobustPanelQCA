#' Calculate Unique Coverage for Terms
#'
#' @description
#' Calculates raw coverage, unique coverage, and consistency for each
#' term in a solution.
#'
#' @param data A data frame with calibrated fuzzy-set variables.
#' @param outcome Character. Name of the outcome variable.
#' @param terms Character vector. Solution terms to analyze.
#'
#' @return A data frame with columns:
#' \describe{
#'   \item{term}{The configuration term}
#'   \item{raw_cov}{Raw coverage of the term}
#'   \item{unique_cov}{Unique coverage (not shared with other terms)}
#'   \item{cons}{Consistency of the term}
#' }
#'
#' @details
#' Raw coverage measures how much of the outcome is covered by each term.
#' Unique coverage measures how much is covered ONLY by that term and
#' not by any other term in the solution.
#'
#' @examples
#' conditions <- c("infrastructure", "knowledge", "finance", "talent")
#' data_cal <- calibrate_panel(
#'   example_panel,
#'   vars = c(conditions, "entrepreneurship")
#' )
#' unique_coverage(
#'   data_cal, "entrepreneurship",
#'   terms = c("infrastructure*knowledge", "finance*talent")
#' )
#'
#' @export
unique_coverage <- function(data, outcome, terms) {
  check_columns(data, outcome, "outcome")
  Y <- data[[outcome]]
  if (length(terms) == 0) return(tibble::tibble())

  Xlist <- purrr::map(terms, ~ term_membership(data, .x))
  names(Xlist) <- terms

  cc <- purrr::map(Xlist, ~ cons_cov_suf(.x, Y))
  raw_cov <- purrr::map_dbl(cc, "coverage")
  cons <- purrr::map_dbl(cc, "consistency")

  unique_cov <- purrr::map_dbl(seq_along(terms), function(i) {
    Xi <- Xlist[[i]]
    if (length(terms) == 1) {
      Xuniq <- Xi
    } else {
      Xothers <- Reduce(pmax, Xlist[-i])
      Xuniq <- pmin(Xi, 1 - Xothers)
    }
    cons_cov_suf(Xuniq, Y)["coverage"]
  })

  tibble::tibble(
    term = terms,
    raw_cov = raw_cov,
    unique_cov = unique_cov,
    cons = cons
  )
}
