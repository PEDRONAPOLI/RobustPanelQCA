#' Example Panel Dataset for fsQCA
#'
#' A simulated panel dataset with 30 cases across 3 time periods,
#' designed to demonstrate panel fsQCA analysis.
#'
#' @format A data frame with 90 rows and 7 variables:
#' \describe{
#'   \item{case_id}{Case identifier (e.g., "case_01")}
#'   \item{period}{Time period (1, 2, or 3)}
#'   \item{infrastructure}{Infrastructure condition (0-100)}
#'   \item{knowledge}{Knowledge/R&D condition (0-100)}
#'   \item{finance}{Financial access condition (0-100)}
#'   \item{talent}{Human capital/talent condition (0-100)}
#'   \item{entrepreneurship}{Outcome variable (0-100)}
#' }
#'
#' @examples
#' data(example_panel)
#' head(example_panel)
#'
#' # Calibrate and run analysis
#' params <- pfsqca_params()
#' conditions <- c("infrastructure", "knowledge", "finance", "talent")
#'
#' data_cal <- calibrate_panel(
#'   example_panel,
#'   vars = c(conditions, "entrepreneurship")
#' )
#'
#' @source Simulated data for demonstration purposes.
"example_panel"
