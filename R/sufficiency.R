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


#' Parse Core and Contributing Conditions from Solution
#'
#' @description
#' Internal function to extract core (parsimonious) and contributing (intermediate only)
#' conditions from QCA solutions.
#'
#' @param pars_terms Character vector. Terms from parsimonious solution.
#' @param int_terms Character vector. Terms from intermediate solution.
#' @param conditions Character vector. All condition names.
#'
#' @return A list with:
#' \describe{
#'   \item{terms}{Character vector of intermediate solution terms}
#'   \item{core_contributing}{Data frame with term, condition, type (core/contributing), and presence (+/-)}
#' }
#'
#' @keywords internal
parse_core_contributing <- function(pars_terms, int_terms, conditions) {

  # Helper to parse a single term into its literals
  parse_term <- function(term) {
    lits <- stringr::str_split(term, "\\*", simplify = TRUE)
    lits <- stringr::str_trim(as.character(lits))
    lits <- lits[lits != ""]
    lits
  }

  # Helper to get condition name without negation
  get_condition_name <- function(lit) {
    if (startsWith(lit, "~")) {
      sub("^~", "", lit)
    } else {
      lit
    }
  }

  # Helper to check if literal is negated
  is_negated <- function(lit) {
    startsWith(lit, "~")
  }

  # Parse parsimonious terms into sets of literals
  pars_literals_list <- lapply(pars_terms, parse_term)

  # For each intermediate term, identify core vs contributing
  results <- list()

  for (int_term in int_terms) {
    int_lits <- parse_term(int_term)

    # Find matching parsimonious term (the one that is a subset)
    matching_pars_idx <- NULL
    for (i in seq_along(pars_literals_list)) {
      pars_lits <- pars_literals_list[[i]]
      # Check if all parsimonious literals are in intermediate term
      if (all(pars_lits %in% int_lits)) {
        matching_pars_idx <- i
        break
      }
    }

    if (is.null(matching_pars_idx)) {
      # No matching parsimonious term found - all are contributing
      core_lits <- character(0)
    } else {
      core_lits <- pars_literals_list[[matching_pars_idx]]
    }

    # Classify each literal
    for (lit in int_lits) {
      cond_name <- get_condition_name(lit)
      presence <- if (is_negated(lit)) "absent" else "present"
      type <- if (lit %in% core_lits) "core" else "contributing"

      results <- c(results, list(data.frame(
        term = int_term,
        condition = cond_name,
        literal = lit,
        presence = presence,
        type = type,
        stringsAsFactors = FALSE
      )))
    }
  }

  if (length(results) > 0) {
    core_contributing <- do.call(rbind, results)
  } else {
    core_contributing <- data.frame(
      term = character(0),
      condition = character(0),
      literal = character(0),
      presence = character(0),
      type = character(0),
      stringsAsFactors = FALSE
    )
  }

  list(
    terms = int_terms,
    core_contributing = tibble::as_tibble(core_contributing)
  )
}


#' Run Sufficiency Analysis
#'
#' @description
#' Performs truth table analysis and Boolean minimization. When directional
#' expectations are provided, returns both parsimonious and intermediate solutions
#' with core and contributing conditions identified.
#'
#' @param data A data frame with calibrated fuzzy-set variables.
#' @param outcome Character. Name of the outcome variable.
#' @param conditions Character vector. Names of condition variables.
#' @param params A pfsqca_params object. If NULL, uses default parameters.
#'
#' @return A list with:
#' \describe{
#'   \item{tt}{The truth table object}
#'   \item{sol_parsimonious}{The parsimonious solution object}
#'   \item{sol_intermediate}{The intermediate solution object (NULL if no dir_exp)}
#'   \item{terms_parsimonious}{Character vector of parsimonious solution terms}
#'   \item{terms_intermediate}{Character vector of intermediate solution terms}
#'   \item{terms}{Character vector of final solution terms (intermediate if available, else parsimonious)}
#'   \item{core_contributing}{Data frame identifying core vs contributing conditions (NULL if no dir_exp)}
#' }
#'
#' @examples
#' \dontrun{
#' # Parsimonious only
#' params <- pfsqca_params()
#' suf <- sufficiency_analysis(data_cal, "outcome", c("A", "B", "C"), params)
#'
#' # With intermediate solution
#' params <- pfsqca_params(dir_exp = c(A = 1, B = 1, C = 0))
#' suf <- sufficiency_analysis(data_cal, "outcome", c("A", "B", "C"), params)
#' suf$core_contributing  # shows core vs contributing
#' }
#'
#' @export
sufficiency_analysis <- function(data, outcome, conditions, params = NULL) {
  if (is.null(params)) params <- pfsqca_params()

  # Create truth table
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

  # Parsimonious solution (always computed)
  sol_pars <- QCA::minimize(
    input = tt,
    include = "?",
    details = TRUE
  )
  terms_pars <- extract_solution_terms(sol_pars)

  # Intermediate solution (only if dir_exp provided)
  sol_int <- NULL
  terms_int <- NULL
  core_contributing <- NULL

  if (!is.null(params$dir_exp)) {
    # Build dir.exp vector in correct order
    dir_exp_vector <- sapply(conditions, function(cond) {
      if (cond %in% names(params$dir_exp)) {
        params$dir_exp[[cond]]
      } else {
        -1  # no expectation for conditions not specified
      }
    })
    names(dir_exp_vector) <- conditions

    sol_int <- QCA::minimize(
      input = tt,
      include = "?",
      dir.exp = dir_exp_vector,
      details = TRUE
    )
    terms_int <- extract_solution_terms(sol_int)

    # Parse core vs contributing
    parsed <- parse_core_contributing(terms_pars, terms_int, conditions)
    core_contributing <- parsed$core_contributing
  }

  # Final terms: use intermediate if available, else parsimonious
  final_terms <- if (!is.null(terms_int)) terms_int else terms_pars

  list(
    tt = tt,
    sol_parsimonious = sol_pars,
    sol_intermediate = sol_int,
    terms_parsimonious = terms_pars,
    terms_intermediate = terms_int,
    terms = final_terms,
    core_contributing = core_contributing
  )
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


#' Create Ball Table for Solution Display
#'
#' @description
#' Creates a data frame formatted for displaying solutions as ball tables,
#' with core conditions (large circles) and contributing conditions (small circles).
#'
#' @param core_contributing Data frame from sufficiency_analysis with core/contributing info.
#' @param conditions Character vector. All condition names in desired order.
#'
#' @return A data frame with one row per term and columns for each condition,
#'   containing symbols: "●" (core present), "◯" (core absent),
#'   "•" (contributing present), "○" (contributing absent), "" (don't care).
#'
#' @examples
#' \dontrun{
#' suf <- sufficiency_analysis(data_cal, "outcome", conditions, params)
#' ball_table <- create_ball_table(suf$core_contributing, conditions)
#' }
#'
#' @export
create_ball_table <- function(core_contributing, conditions) {
  if (is.null(core_contributing) || nrow(core_contributing) == 0) {
    return(tibble::tibble())
  }

  terms <- unique(core_contributing$term)

  result <- tibble::tibble(Path = paste0("Path ", seq_along(terms)), Term = terms)

  for (cond in conditions) {
    result[[cond]] <- sapply(terms, function(trm) {
      row <- core_contributing %>%
        dplyr::filter(term == trm, condition == cond)

      if (nrow(row) == 0) {
        return("")  # don't care
      }

      type <- row$type[1]
      presence <- row$presence[1]

      if (type == "core") {
        if (presence == "present") return("\u25CF")  # ● large filled
        else return("\u2297")  # ⊗ large crossed
      } else {
        if (presence == "present") return("\u2022")  # • small filled
        else return("\u2298")  # ⊘ small crossed
      }
    })
  }

  result
}
