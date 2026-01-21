#' Robustness Test for Sufficiency (Random Deletion)
#'
#' @description
#' Tests robustness of sufficient configurations by randomly deleting
#' observations and re-running the analysis multiple times.
#'
#' @param data A data frame with calibrated fuzzy-set variables.
#' @param outcome Character. Name of the outcome variable.
#' @param conditions Character vector. Names of condition variables.
#' @param params A pfsqca_params object. If NULL, uses default parameters.
#'
#' @return A list with:
#' \describe{
#'   \item{baseline_terms}{Terms from the original analysis}
#'   \item{freq_tbl}{Data frame with term frequencies across iterations}
#' }
#'
#' @details
#' For each iteration, a proportion of cases (defined by params$robustness_drop)
#' is randomly removed, and sufficiency analysis is re-run. The frequency
#' table shows how often each term appears across all iterations.
#'
#' Terms appearing in >= params$robust_PI_freq_cut of iterations are
#' considered robust.
#'
#' @examples
#' \dontrun{
#' params <- pfsqca_params(robustness_B = 100)  # fewer iterations for testing
#' rob <- sufficiency_robustness(data_cal, "outcome", conditions, params)
#' rob$freq_tbl  # see which terms are robust
#' }
#'
#' @export
sufficiency_robustness <- function(data, outcome, conditions, params = NULL) {
  if (is.null(params)) params <- pfsqca_params()

  set.seed(params$robustness_seed)
  n <- nrow(data)
  keep_n <- max(2, floor(n * (1 - params$robustness_drop)))

  # Baseline run
  base_run <- sufficiency_analysis(data, outcome, conditions, params)
  base_terms <- unique(base_run$terms)

  # Random deletion iterations
  all_terms <- vector("list", params$robustness_B)
  for (b in seq_len(params$robustness_B)) {
    idx <- sample.int(n, size = keep_n, replace = FALSE)
    dsub <- data[idx, , drop = FALSE]
    run <- tryCatch(
      sufficiency_analysis(dsub, outcome, conditions, params),
      error = function(e) NULL
    )
    if (is.null(run)) {
      all_terms[[b]] <- character(0)
    } else {
      all_terms[[b]] <- unique(run$terms)
    }
  }

  # Frequency table
  freq_tbl <- tibble::tibble(term = unlist(all_terms, use.names = FALSE)) %>%
    dplyr::filter(!is.na(term), term != "") %>%
    dplyr::count(term, sort = TRUE) %>%
    dplyr::mutate(
      freq = n / params$robustness_B,
      in_baseline = term %in% base_terms
    )

  list(baseline_terms = base_terms, freq_tbl = freq_tbl)
}


#' Robustness Test for Necessity (Random Deletion)
#'
#' @description
#' Tests robustness of necessity findings by randomly deleting
#' observations and re-testing necessity multiple times.
#'
#' @param data A data frame with calibrated fuzzy-set variables.
#' @param outcome Character. Name of the outcome variable.
#' @param conditions Character vector. Names of condition variables.
#' @param params A pfsqca_params object. If NULL, uses default parameters.
#'
#' @return A data frame with:
#' \describe{
#'   \item{condition}{The condition tested}
#'   \item{robust_share}{Proportion of iterations where condition was necessary}
#'   \item{robust_nec_flag}{Whether robust_share >= params$robust_nec_cut}
#' }
#'
#' @examples
#' \dontrun{
#' params <- pfsqca_params(robustness_B = 100)
#' rob_nec <- necessity_robustness(data_cal, "outcome", conditions, params)
#' rob_nec  # see which conditions are robustly necessary
#' }
#'
#' @export
necessity_robustness <- function(data, outcome, conditions, params = NULL) {
  if (is.null(params)) params <- pfsqca_params()

  set.seed(params$robustness_seed)
  n <- nrow(data)
  keep_n <- max(2, floor(n * (1 - params$robustness_drop)))

  conds_all <- c(conditions, paste0("~", conditions))
  hits <- matrix(0L, nrow = params$robustness_B, ncol = length(conds_all))
  colnames(hits) <- conds_all

  for (b in seq_len(params$robustness_B)) {
    idx <- sample.int(n, size = keep_n, replace = FALSE)
    dsub <- data[idx, , drop = FALSE]
    Y <- dsub[[outcome]]
    for (j in seq_along(conds_all)) {
      cond <- conds_all[j]
      X <- literal_membership(dsub, cond)
      inclN <- cons_cov_nec(X, Y)["inclN"]
      hits[b, j] <- as.integer(is.finite(inclN) && inclN >= params$nec_threshold)
    }
  }

  tibble::tibble(
    condition = conds_all,
    robust_share = colMeans(hits),
    robust_nec_flag = colMeans(hits) >= params$robust_nec_cut
  ) %>%
    dplyr::arrange(dplyr::desc(robust_share))
}
