#' Consistency and Coverage for Sufficiency
#'
#' @description
#' Calculates consistency and coverage for a sufficiency relation (X → Y).
#'
#' @param X Numeric vector. Membership scores for the condition/configuration.
#' @param Y Numeric vector. Membership scores for the outcome.
#'
#' @return A named numeric vector with elements \code{consistency} and \code{coverage}.
#'
#' @details
#' For sufficiency (X is subset of Y):
#' \itemize{
#'   \item Consistency = sum(min(X,Y)) / sum(X)
#'   \item Coverage = sum(min(X,Y)) / sum(Y)
#' }
#'
#' @examples
#' X <- c(0.2, 0.5, 0.8, 0.9)
#' Y <- c(0.3, 0.6, 0.7, 0.95)
#' cons_cov_suf(X, Y)
#'
#' @export
cons_cov_suf <- function(X, Y) {
  num <- sum(pmin(X, Y), na.rm = TRUE)
  cons <- num / sum(X, na.rm = TRUE)
  cov <- num / sum(Y, na.rm = TRUE)
  c(consistency = cons, coverage = cov)
}


#' Consistency and Coverage for Necessity
#'
#' @description
#' Calculates consistency and coverage for a necessity relation (Y → X).
#'
#' @param X Numeric vector. Membership scores for the condition.
#' @param Y Numeric vector. Membership scores for the outcome.
#'
#' @return A named numeric vector with elements \code{inclN} (inclusion/consistency)
#'   and \code{covN} (coverage).
#'
#' @details
#' For necessity (Y is subset of X):
#' \itemize{
#'   \item Inclusion (consistency) = sum(min(X,Y)) / sum(Y)
#'   \item Coverage = sum(min(X,Y)) / sum(X)
#' }
#'
#' @examples
#' X <- c(0.2, 0.5, 0.8, 0.9)
#' Y <- c(0.3, 0.6, 0.7, 0.95)
#' cons_cov_nec(X, Y)
#'
#' @export
cons_cov_nec <- function(X, Y) {
  num <- sum(pmin(X, Y), na.rm = TRUE)
  inclN <- num / sum(Y, na.rm = TRUE)
  covN <- num / sum(X, na.rm = TRUE)
  c(inclN = inclN, covN = covN)
}
