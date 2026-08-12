#' Check That Columns Exist in a Data Frame
#'
#' @description
#' Internal helper that produces an actionable error message when a column
#' name is misspelled or missing, listing the columns that are available.
#'
#' @param data A data frame.
#' @param vars Character vector of column names that must be present.
#' @param what Character. Label used in the error message (e.g. "condition").
#'
#' @return Invisibly `TRUE`, or an error.
#'
#' @keywords internal
check_columns <- function(data, vars, what = "column") {
  if (!is.data.frame(data)) {
    stop("`data` must be a data frame, not a ", class(data)[1], ".", call. = FALSE)
  }
  missing_vars <- setdiff(vars, names(data))
  if (length(missing_vars) > 0) {
    stop(
      sprintf(
        "%s not found in the data: %s\n  Columns available: %s\n  Check the spelling (R is case-sensitive: \"Period\" is not \"period\").",
        if (length(missing_vars) == 1) paste0(toupper(substring(what, 1, 1)), substring(what, 2)) else paste0(what, "s"),
        paste0("\"", missing_vars, "\"", collapse = ", "),
        paste0("\"", names(data), "\"", collapse = ", ")
      ),
      call. = FALSE
    )
  }
  invisible(TRUE)
}


#' Check That Variables Look Calibrated
#'
#' @description
#' Internal helper that warns when variables passed to an analysis function do
#' not look like fuzzy-set membership scores. Every consistency and coverage
#' formula in fsQCA assumes values in the 0-1 interval; feeding raw values
#' (say, 0-100 scores) produces numbers that look fine but mean nothing.
#'
#' @param data A data frame.
#' @param vars Character vector of variable names to check.
#' @param action Character. Either "warn" (default) or "error".
#'
#' @return Invisibly `TRUE`.
#'
#' @keywords internal
check_calibrated <- function(data, vars, action = c("warn", "error")) {
  action <- match.arg(action)
  vars <- intersect(vars, names(data))

  bad <- vars[vapply(vars, function(v) {
    x <- data[[v]]
    if (!is.numeric(x)) return(TRUE)
    rng <- range(x, na.rm = TRUE)
    !all(is.finite(rng)) || rng[1] < 0 || rng[2] > 1
  }, logical(1))]

  if (length(bad) > 0) {
    msg <- sprintf(
      "These variables are not fuzzy-set scores in [0, 1]: %s\n  fsQCA consistency and coverage are only meaningful on calibrated data.\n  Did you forget calibrate_panel()? Example:\n    data_cal <- calibrate_panel(data, vars = c(conditions, outcome))",
      paste0("\"", bad, "\"", collapse = ", ")
    )
    if (action == "error") stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  }

  invisible(TRUE)
}
