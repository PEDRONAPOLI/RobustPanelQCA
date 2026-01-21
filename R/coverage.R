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
#' \dontrun{
#' suf <- sufficiency_analysis(data_cal, "outcome", conditions, params)
#' cov <- unique_coverage(data_cal, "outcome", suf$terms)
#' cov  # see coverage breakdown
#' }
#'
#' @export
unique_coverage <- function(data, outcome, terms) {
  Y <- data[[outcome]]
  if (length(terms) == 0) return(tibble::tibble())

  Xlist <- purrr::map(terms, ~ term_membership(data, .x))
  names(Xlist) <- terms

  raw_cov <- purrr::map_dbl(Xlist, ~ cons_cov_suf(.x, Y)["coverage"])
  cons <- purrr::map_dbl(Xlist, ~ cons_cov_suf(.x, Y)["consistency"])

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
