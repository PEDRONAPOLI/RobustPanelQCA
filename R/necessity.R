#' Test Necessity of Conditions
#'
#' @description
#' Tests whether conditions are necessary for the outcome.
#'
#' @param data A data frame with calibrated fuzzy-set variables.
#' @param outcome Character. Name of the outcome variable.
#' @param conditions Character vector. Names of condition variables.
#' @param params A pfsqca_params object. If NULL, uses default parameters.
#'
#' @return A data frame with necessity test results for each condition
#'   (both presence and absence), including inclN, covN, and necessity flag.
#'
#' @examples
#' data_cal <- calibrate_panel(
#'   example_panel,
#'   vars = c("infrastructure", "knowledge", "entrepreneurship")
#' )
#' necessity_test(
#'   data_cal, "entrepreneurship", c("infrastructure", "knowledge")
#' )
#'
#' @export
necessity_test <- function(data, outcome, conditions, params = NULL) {
  if (is.null(params)) params <- pfsqca_params()
  check_columns(data, c(outcome, conditions), "variable")
  check_calibrated(data, c(outcome, conditions))

  Y <- data[[outcome]]
  conds_all <- c(conditions, paste0("~", conditions))

  result <- purrr::map_dfr(conds_all, function(cond) {
    X <- literal_membership(data, cond)
    cc <- cons_cov_nec(X, Y)
    tibble::tibble(
      outcome = outcome,
      condition = cond,
      inclN = as.numeric(cc["inclN"]),
      covN = as.numeric(cc["covN"]),
      nec_flag = as.numeric(cc["inclN"]) >= params$nec_threshold
    )
  })

  dplyr::arrange(result, dplyr::desc(inclN))
}


#' Panel Diagnostics for Necessity
#'
#' @description
#' Calculates between-period and within-case necessity metrics
#' to assess stability of necessity relations across time and units.
#'
#' @param data A data frame with calibrated fuzzy-set variables.
#' @param outcome Character. Name of the outcome variable.
#' @param conditions Character vector. Names of condition variables.
#' @param id_var Character. Name of the case identifier variable.
#'   Default is "case_id".
#' @param time_var Character. Name of the time variable. Default is "period".
#'
#' @return A list with two data frames:
#' \describe{
#'   \item{between}{Necessity metrics by time period (BECONS_N, BECOV_N)}
#'   \item{within}{Necessity metrics by case (WICONS_N, WICOV_N)}
#' }
#' The grouping column keeps the name you passed in `time_var` / `id_var`.
#'
#' @examples
#' data_cal <- calibrate_panel(
#'   example_panel,
#'   vars = c("infrastructure", "knowledge", "entrepreneurship")
#' )
#' diag <- necessity_panel_diagnostics(
#'   data_cal, "entrepreneurship", c("infrastructure", "knowledge")
#' )
#' head(diag$between)  # by period
#' head(diag$within)   # by case
#'
#' @export
necessity_panel_diagnostics <- function(data, outcome, conditions,
                                        id_var = "case_id", time_var = "period") {
  check_columns(data, c(outcome, conditions), "variable")
  check_columns(data, time_var, "time variable")
  check_columns(data, id_var, "case identifier")
  check_calibrated(data, c(outcome, conditions))

  Y <- data[[outcome]]
  conds_all <- c(conditions, paste0("~", conditions))

  between <- purrr::map_dfr(conds_all, function(cond) {
    Xfull <- literal_membership(data, cond)
    data %>%
      dplyr::mutate(.X = Xfull, .Y = Y) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(time_var))) %>%
      dplyr::summarise(
        n = dplyr::n(),
        BECONS_N = cons_cov_nec(.X, .Y)["inclN"],
        BECOV_N = cons_cov_nec(.X, .Y)["covN"],
        .groups = "drop"
      ) %>%
      dplyr::mutate(condition = cond)
  })

  within <- purrr::map_dfr(conds_all, function(cond) {
    Xfull <- literal_membership(data, cond)
    data %>%
      dplyr::mutate(.X = Xfull, .Y = Y) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(id_var))) %>%
      dplyr::summarise(
        n = dplyr::n(),
        WICONS_N = cons_cov_nec(.X, .Y)["inclN"],
        WICOV_N = cons_cov_nec(.X, .Y)["covN"],
        .groups = "drop"
      ) %>%
      dplyr::mutate(condition = cond)
  })

  list(between = between, within = within)
}
