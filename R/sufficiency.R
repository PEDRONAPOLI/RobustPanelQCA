#' Extract Solution Terms from QCA Solution Object
#'
#' @description
#' Internal function to extract prime implicant terms from a QCA solution.
#'
#' @param sol_obj A solution object from QCA::minimize.
#'
#' @return Character vector of solution terms.
#'
#' @keywords internal
extract_solution_terms <- function(sol_obj) {
  out <- character(0)
  if (!is.null(sol_obj$solution)) {
    if (is.character(sol_obj$solution)) out <- unique(c(out, sol_obj$solution))
    if (is.list(sol_obj$solution)) out <- unique(c(out, unlist(sol_obj$solution, use.names = FALSE)))
  }
  out <- stringr::str_trim(out)
  out <- out[out != ""]
  out
}


#' Run Sufficiency Analysis
#'
#' @description
#' Performs truth table analysis and Boolean minimization.
#'
#' @param data A data frame with calibrated fuzzy-set variables.
#' @param outcome Character. Name of the outcome variable.
#' @param conditions Character vector. Names of condition variables.
#' @param params A pfsqca_params object. If NULL, uses default parameters.
#'
#' @return A list with:
#' \describe{
#'   \item{tt}{The truth table object}
#'   \item{sol}{The minimized solution object}
#'   \item{terms}{Character vector of solution terms}
#' }
#'
#' @examples
#' \dontrun{
#' params <- pfsqca_params()
#' suf <- sufficiency_analysis(data_cal, "outcome", c("A", "B", "C"), params)
#' suf$terms  # view solution terms
#' }
#'
#' @export
sufficiency_analysis <- function(data, outcome, conditions, params = NULL) {
  if (is.null(params)) params <- pfsqca_params()

  tt <- QCA::truthTable(
    data = data,
    outcome = outcome,
    conditions = conditions,
    incl.cut = params$incl_cut,
    pri.cut = params$pri_cut,
    n.cut = params$n_cut,
    complete = TRUE,
    show.cases = TRUE
  )

  sol <- QCA::minimize(input = tt, include = "?", details = TRUE)
  terms <- extract_solution_terms(sol)

  list(tt = tt, sol = sol, terms = terms)
}


#' Panel Metrics for Sufficiency
#'
#' @description
#' Calculates pooled, between-period, and within-case consistency and coverage
#' metrics for sufficient configurations.
#'
#' @param data A data frame with calibrated fuzzy-set variables.
#' @param outcome Character. Name of the outcome variable.
#' @param sol_terms Character vector. Solution terms from sufficiency analysis.
#' @param id_var Character. Name of the case identifier variable. Default is "MSA".
#' @param time_var Character. Name of the time variable. Default is "year".
#'
#' @return A list with three data frames:
#' \describe{
#'   \item{pooled}{POCONS and POCOV for each term and overall solution}
#'   \item{between}{BECONS and BECOV by time period}
#'   \item{within}{WICONS and WICOV by case}
#' }
#'
#' @examples
#' \dontrun{
#' suf <- sufficiency_analysis(data_cal, "outcome", conditions, params)
#' pm <- panel_metrics(data_cal, "outcome", suf$terms)
#' pm$pooled   # overall metrics
#' pm$between  # by year
#' pm$within   # by case
#' }
#'
#' @export
panel_metrics <- function(data, outcome, sol_terms, id_var = "MSA", time_var = "year") {
  Y <- data[[outcome]]

  # Pooled metrics for each term
  term_tbl <- purrr::map_dfr(sol_terms, function(trm) {
    X <- term_membership(data, trm)
    cc <- cons_cov_suf(X, Y)
    tibble::tibble(
      component = "term",
      config = trm,
      POCONS = cc["consistency"],
      POCOV = cc["coverage"]
    )
  })

  # Pooled metrics for overall solution
  Xsol <- solution_membership(data, sol_terms)
  cc_sol <- cons_cov_suf(Xsol, Y)
  sol_row <- tibble::tibble(
    component = "solution",
    config = "SOLUTION (OR of terms)",
    POCONS = cc_sol["consistency"],
    POCOV = cc_sol["coverage"]
  )
  pooled <- dplyr::bind_rows(sol_row, term_tbl)

  # Between metrics (by time period)
  by_year <- data %>%
    dplyr::mutate(.Y = Y, .Xsol = Xsol) %>%
    dplyr::group_by(.data[[time_var]]) %>%
    dplyr::summarise(
      n = dplyr::n(),
      BECONS = cons_cov_suf(.Xsol, .Y)["consistency"],
      BECOV = cons_cov_suf(.Xsol, .Y)["coverage"],
      .groups = "drop"
    ) %>%
    dplyr::rename(year = .data[[time_var]])

  # Within metrics (by case)
  by_case <- data %>%
    dplyr::mutate(.Y = Y, .Xsol = Xsol) %>%
    dplyr::group_by(.data[[id_var]]) %>%
    dplyr::summarise(
      n = dplyr::n(),
      WICONS = cons_cov_suf(.Xsol, .Y)["consistency"],
      WICOV = cons_cov_suf(.Xsol, .Y)["coverage"],
      .groups = "drop"
    ) %>%
    dplyr::rename(case_id = .data[[id_var]])

  list(pooled = pooled, between = by_year, within = by_case)
}
