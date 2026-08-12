#' Membership in a Single Literal
#'
#' @description
#' Calculates membership scores for a single literal (condition or its negation).
#'
#' @param data A data frame with calibrated fuzzy-set variables.
#' @param lit Character. The literal name. Use "~" prefix for negation
#'   (e.g., "~income" for absence of income).
#'
#' @return Numeric vector of membership scores.
#'
#' @details
#' For a positive literal (e.g., "income"), returns the values as-is.
#' For a negated literal (e.g., "~income"), returns 1 - values.
#'
#' @examples
#' data_cal <- calibrate_panel(example_panel, vars = "infrastructure")
#' head(literal_membership(data_cal, "infrastructure"))   # presence
#' head(literal_membership(data_cal, "~infrastructure"))  # absence
#'
#' @export
literal_membership <- function(data, lit) {
  lit <- stringr::str_trim(lit)
  if (startsWith(lit, "~")) {
    1 - data[[sub("^~", "", lit)]]
  } else {
    data[[lit]]
  }
}


#' Membership in a Term (Conjunction)
#'
#' @description
#' Calculates membership scores for a term, which is a conjunction (AND)
#' of multiple literals.
#'
#' @param data A data frame with calibrated fuzzy-set variables.
#' @param term Character. The term expressed as literals joined by "*"
#'   (e.g., "income*patents*~density").
#'
#' @return Numeric vector of membership scores (minimum across all literals).
#'
#' @details
#' In fuzzy-set logic, conjunction (AND) is calculated as the minimum
#' of membership scores across all literals in the term.
#'
#' @examples
#' data_cal <- calibrate_panel(
#'   example_panel,
#'   vars = c("infrastructure", "knowledge", "finance")
#' )
#' head(term_membership(data_cal, "infrastructure*knowledge"))
#' head(term_membership(data_cal, "infrastructure*~finance"))
#'
#' @export
term_membership <- function(data, term) {
  lits <- stringr::str_split(term, "\\*", simplify = TRUE)
  lits <- stringr::str_trim(as.character(lits))
  mems <- purrr::map(lits, ~ literal_membership(data, .x))
  Reduce(pmin, mems)
}


#' Membership in a Solution (Disjunction)
#'
#' @description
#' Calculates membership scores for a solution, which is a disjunction (OR)
#' of multiple terms.
#'
#' @param data A data frame with calibrated fuzzy-set variables.
#' @param terms Character vector. Each element is a term (conjunction of literals).
#'
#' @return Numeric vector of membership scores (maximum across all terms).
#'
#' @details
#' In fuzzy-set logic, disjunction (OR) is calculated as the maximum
#' of membership scores across all terms in the solution.
#'
#' A solution like "A*B + C*~D" would be passed as:
#' \code{terms = c("A*B", "C*~D")}
#'
#' @examples
#' conditions <- c("infrastructure", "knowledge", "finance", "talent")
#' data_cal <- calibrate_panel(example_panel, vars = conditions)
#' head(solution_membership(
#'   data_cal, c("infrastructure*knowledge", "finance*talent")
#' ))
#'
#' @export
solution_membership <- function(data, terms) {
  if (length(terms) == 0) return(rep(NA_real_, nrow(data)))
  mems <- purrr::map(terms, ~ term_membership(data, .x))
  Reduce(pmax, mems)
}
