#' Run the Complete Panel fsQCA Workflow
#'
#' @description
#' Runs every step of a panel fsQCA analysis in one call: calibration,
#' necessity, necessity panel diagnostics, necessity robustness, sufficiency
#' (truth table and minimization), panel metrics, unique coverage, and
#' sufficiency robustness.
#'
#' This is the function behind the fill-in-the-blanks script produced by
#' [use_pfsqca_template()]. If you want to inspect or tweak individual steps,
#' call the underlying functions directly.
#'
#' @param data A data frame in long panel format: one row per case-period, one
#'   column per condition, plus a case identifier and a time column.
#' @param outcome Character. Name of the outcome column.
#' @param conditions Character vector. Names of the condition columns.
#' @param id_var Character. Name of the case identifier column.
#'   Default is "case_id".
#' @param time_var Character. Name of the time column. Default is "period".
#' @param params A `pfsqca_params` object. If NULL, uses [pfsqca_params()]
#'   defaults.
#' @param calibrate Logical. If TRUE (default), raw values are calibrated with
#'   [calibrate_panel()]. Set to FALSE when your data already holds fuzzy-set
#'   scores in the 0-1 range.
#' @param probs Numeric vector of length 3. Calibration percentiles passed to
#'   [calibrate_panel()]. Ignored when `calibrate = FALSE`.
#' @param robustness Logical. If TRUE (default), runs the random deletion
#'   robustness tests. These are the slowest part of the analysis; set to FALSE
#'   for a quick first look.
#' @param verbose Logical. If TRUE (default), prints progress messages.
#'
#' @return An object of class `pfsqca_result`: a list with the analysis inputs
#'   (`data_raw`, `data_cal`, `params`, `outcome`, `conditions`, `id_var`,
#'   `time_var`) and every result table (`necessity`, `necessity_panel`,
#'   `necessity_robustness`, `sufficiency`, `terms`, `panel_metrics`,
#'   `coverage`, `sufficiency_robustness`, `ball_table`, `solution_type`).
#'
#' @seealso [use_pfsqca_template()] to get a ready-to-edit script,
#'   [export_pfsqca()] to write the tables to CSV, and [pfsqca_report()] to
#'   render an HTML report.
#'
#' @examples
#' \donttest{
#' conditions <- c("infrastructure", "knowledge", "finance", "talent")
#' res <- run_pfsqca(
#'   data       = example_panel,
#'   outcome    = "entrepreneurship",
#'   conditions = conditions,
#'   id_var     = "case_id",
#'   time_var   = "period",
#'   params     = pfsqca_params(incl_cut = 0.80, n_cut = 1, robustness_B = 50)
#' )
#' res
#' res$panel_metrics$pooled
#' }
#'
#' @export
run_pfsqca <- function(data,
                       outcome,
                       conditions,
                       id_var = "case_id",
                       time_var = "period",
                       params = NULL,
                       calibrate = TRUE,
                       probs = c(0.10, 0.50, 0.90),
                       robustness = TRUE,
                       verbose = TRUE) {

  if (is.null(params)) params <- pfsqca_params()
  if (!inherits(params, "pfsqca_params")) {
    stop("`params` must be created with pfsqca_params().", call. = FALSE)
  }

  data <- as.data.frame(data)
  check_columns(data, c(outcome, conditions), "variable")
  check_columns(data, id_var, "case identifier")
  check_columns(data, time_var, "time variable")

  if (length(conditions) < 2) {
    stop("At least two conditions are needed for a configurational analysis.",
         call. = FALSE)
  }
  if (outcome %in% conditions) {
    stop("The outcome (\"", outcome, "\") is also listed as a condition. ",
         "Remove it from `conditions`.", call. = FALSE)
  }

  say <- function(...) if (isTRUE(verbose)) message(...)

  # --- Step 1: calibration ------------------------------------------------
  if (isTRUE(calibrate)) {
    say("[1/6] Calibrating ", length(conditions) + 1, " variables ",
        "(percentiles ", paste(probs, collapse = "/"), ") ...")
    data_cal <- calibrate_panel(data, vars = c(conditions, outcome), probs = probs)
  } else {
    say("[1/6] Skipping calibration (calibrate = FALSE); data assumed to be fuzzy sets ...")
    data_cal <- data
    check_calibrated(data_cal, c(conditions, outcome), action = "error")
  }

  # --- Step 2: necessity --------------------------------------------------
  say("[2/6] Testing necessity ...")
  necessity <- necessity_test(data_cal, outcome, conditions, params)
  necessity_panel <- necessity_panel_diagnostics(
    data_cal, outcome, conditions, id_var = id_var, time_var = time_var
  )

  # --- Step 3: necessity robustness ---------------------------------------
  necessity_rob <- NULL
  if (isTRUE(robustness)) {
    say("[3/6] Necessity robustness (", params$robustness_B, " random deletions) ...")
    necessity_rob <- necessity_robustness(data_cal, outcome, conditions, params)
  } else {
    say("[3/6] Skipping necessity robustness (robustness = FALSE) ...")
  }

  # --- Step 4: sufficiency ------------------------------------------------
  say("[4/6] Building truth table and minimizing ...")
  sufficiency <- sufficiency_analysis(data_cal, outcome, conditions, params)
  terms <- sufficiency$terms

  if (length(terms) == 0) {
    stop(
      "The minimization returned no solution term.\n  Try lowering incl_cut or n_cut in pfsqca_params().",
      call. = FALSE
    )
  }
  say("      Found ", length(terms), " path(s): ", paste(terms, collapse = " + "))

  ball_table <- NULL
  if (!is.null(sufficiency$core_contributing)) {
    ball_table <- create_ball_table(sufficiency$core_contributing, conditions)
  }

  # --- Step 5: panel metrics and coverage ---------------------------------
  say("[5/6] Computing panel metrics (POCONS / BECONS / WICONS) ...")
  pm <- panel_metrics(data_cal, outcome, terms, id_var = id_var, time_var = time_var)
  coverage <- unique_coverage(data_cal, outcome, terms)

  # --- Step 6: sufficiency robustness -------------------------------------
  sufficiency_rob <- NULL
  if (isTRUE(robustness)) {
    say("[6/6] Sufficiency robustness (", params$robustness_B,
        " random deletions; this is the slow step) ...")
    sufficiency_rob <- sufficiency_robustness(data_cal, outcome, conditions, params)
  } else {
    say("[6/6] Skipping sufficiency robustness (robustness = FALSE) ...")
  }

  say("Done.")

  structure(
    list(
      data_raw               = data,
      data_cal               = data_cal,
      params                 = params,
      outcome                = outcome,
      conditions             = conditions,
      id_var                 = id_var,
      time_var               = time_var,
      calibrated             = isTRUE(calibrate),
      necessity              = necessity,
      necessity_panel        = necessity_panel,
      necessity_robustness   = necessity_rob,
      sufficiency            = sufficiency,
      terms                  = terms,
      solution_type          = if (!is.null(params$dir_exp)) "intermediate" else "parsimonious",
      panel_metrics          = pm,
      coverage               = coverage,
      sufficiency_robustness = sufficiency_rob,
      ball_table             = ball_table,
      run_at                 = Sys.time()
    ),
    class = "pfsqca_result"
  )
}


#' Print a Panel fsQCA Result
#'
#' @param x A `pfsqca_result` object from [run_pfsqca()].
#' @param digits Integer. Number of digits to display. Default is 3.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns `x`.
#'
#' @examples
#' \donttest{
#' res <- run_pfsqca(
#'   example_panel, "entrepreneurship",
#'   c("infrastructure", "knowledge", "finance", "talent"),
#'   params = pfsqca_params(incl_cut = 0.80, n_cut = 1),
#'   robustness = FALSE, verbose = FALSE
#' )
#' print(res)
#' }
#'
#' @export
print.pfsqca_result <- function(x, digits = 3, ...) {
  rnd <- function(v) round(as.numeric(v), digits)

  cat("Panel fsQCA result\n")
  cat("==================\n\n")
  cat("Outcome    :", x$outcome, "\n")
  cat("Conditions :", paste(x$conditions, collapse = ", "), "\n")
  cat("Panel      :", length(unique(x$data_raw[[x$id_var]])), "cases x",
      length(unique(x$data_raw[[x$time_var]])), "periods =",
      nrow(x$data_raw), "observations\n")
  cat("Solution   :", x$solution_type, "\n\n")

  # Necessity
  cat("NECESSARY CONDITIONS (inclN >=", x$params$nec_threshold, ")\n")
  nec <- x$necessity[x$necessity$nec_flag, , drop = FALSE]
  if (nrow(nec) == 0) {
    cat("  None. No condition is necessary for the outcome.\n")
    cat("  Highest inclN: ", x$necessity$condition[1], " = ",
        rnd(x$necessity$inclN[1]), "\n", sep = "")
  } else {
    for (i in seq_len(nrow(nec))) {
      cat(sprintf("  %-20s inclN = %s, covN = %s\n",
                  nec$condition[i], rnd(nec$inclN[i]), rnd(nec$covN[i])))
    }
  }

  if (!is.null(x$necessity_robustness)) {
    rob_nec <- x$necessity_robustness[x$necessity_robustness$robust_nec_flag, , drop = FALSE]
    cat(sprintf("  Robustly necessary (share >= %s): %s\n",
                x$params$robust_nec_cut,
                if (nrow(rob_nec) == 0) "none" else paste(rob_nec$condition, collapse = ", ")))
  }

  # Sufficiency
  cat("\nSUFFICIENT PATHS\n")
  cov_tbl <- x$coverage
  for (i in seq_along(x$terms)) {
    trm <- x$terms[i]
    row <- cov_tbl[cov_tbl$term == trm, , drop = FALSE]
    cat(sprintf("  %d. %s\n", i, trm))
    if (nrow(row) == 1) {
      cat(sprintf("       consistency = %s | raw coverage = %s | unique coverage = %s\n",
                  rnd(row$cons), rnd(row$raw_cov), rnd(row$unique_cov)))
    }
  }

  pooled <- x$panel_metrics$pooled
  sol <- pooled[pooled$component == "solution", , drop = FALSE]
  cat(sprintf("\n  Overall solution: POCONS = %s | POCOV = %s\n",
              rnd(sol$POCONS[1]), rnd(sol$POCOV[1])))

  # Panel stability
  be <- x$panel_metrics$between
  wi <- x$panel_metrics$within
  cat("\nPANEL STABILITY\n")
  cat(sprintf("  Between periods (BECONS): min = %s, mean = %s, max = %s\n",
              rnd(min(be$BECONS, na.rm = TRUE)),
              rnd(mean(be$BECONS, na.rm = TRUE)),
              rnd(max(be$BECONS, na.rm = TRUE))))
  cat(sprintf("  Within cases   (WICONS): mean = %s, share >= 0.70 = %s\n",
              rnd(mean(wi$WICONS, na.rm = TRUE)),
              rnd(mean(wi$WICONS >= 0.70, na.rm = TRUE))))

  # Sufficiency robustness
  if (!is.null(x$sufficiency_robustness)) {
    ft <- x$sufficiency_robustness$freq_tbl
    robust <- ft[ft$freq >= x$params$robust_PI_freq_cut, , drop = FALSE]
    cat("\nSUFFICIENCY ROBUSTNESS (", x$params$robustness_B, " random deletions of ",
        x$params$robustness_drop * 100, "% of cases)\n", sep = "")
    pct <- function(v) sprintf("%.0f%%", 100 * as.numeric(v))
    if (nrow(robust) == 0) {
      cat("  No path survived in >= ", pct(x$params$robust_PI_freq_cut),
          " of iterations.\n", sep = "")
      if (nrow(ft) > 0) {
        cat(sprintf("  Most frequent: %s (%s)\n", ft$term[1], pct(ft$freq[1])))
      }
    } else {
      for (i in seq_len(nrow(robust))) {
        cat(sprintf("  %-32s appears in %s of iterations%s\n",
                    robust$term[i], pct(robust$freq[i]),
                    if (isTRUE(robust$in_baseline[i])) " (in baseline)" else " (NOT in baseline)"))
      }
    }
  }

  cat("\nFull tables: names(x). Write them to disk with export_pfsqca(x).\n")
  invisible(x)
}


#' Export Panel fsQCA Results to CSV Files
#'
#' @description
#' Writes every result table of a [run_pfsqca()] analysis to a folder as CSV
#' files, plus a plain-text record of the parameters used. Handy for opening
#' results in Excel or attaching them to a paper's replication package.
#'
#' @param x A `pfsqca_result` object from [run_pfsqca()].
#' @param dir Character. Folder to write into. Created if it does not exist.
#'   Default is "pfsqca_results".
#' @param quiet Logical. If TRUE, suppresses the progress message.
#'
#' @return Invisibly, the character vector of files written.
#'
#' @examples
#' \donttest{
#' res <- run_pfsqca(
#'   example_panel, "entrepreneurship",
#'   c("infrastructure", "knowledge", "finance", "talent"),
#'   params = pfsqca_params(incl_cut = 0.80, n_cut = 1),
#'   robustness = FALSE, verbose = FALSE
#' )
#' out <- file.path(tempdir(), "results")
#' export_pfsqca(res, dir = out)
#' }
#'
#' @export
export_pfsqca <- function(x, dir = "pfsqca_results", quiet = FALSE) {
  if (!inherits(x, "pfsqca_result")) {
    stop("`x` must be a pfsqca_result object created by run_pfsqca().", call. = FALSE)
  }
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  tables <- list(
    "01_necessity"                = x$necessity,
    "02_necessity_between_period" = x$necessity_panel$between,
    "03_necessity_within_case"    = x$necessity_panel$within,
    "04_necessity_robustness"     = x$necessity_robustness,
    "05_solution_paths"           = x$coverage,
    "06_panel_pooled"             = x$panel_metrics$pooled,
    "07_panel_between_period"     = x$panel_metrics$between,
    "08_panel_within_case"        = x$panel_metrics$within,
    "09_sufficiency_robustness"   = if (is.null(x$sufficiency_robustness)) NULL else x$sufficiency_robustness$freq_tbl,
    "10_core_contributing"        = x$sufficiency$core_contributing,
    "11_ball_table"               = x$ball_table,
    "12_calibrated_data"          = x$data_cal
  )

  written <- character(0)
  for (nm in names(tables)) {
    tbl <- tables[[nm]]
    if (is.null(tbl) || nrow(tbl) == 0) next
    path <- file.path(dir, paste0(nm, ".csv"))
    utils::write.csv(as.data.frame(tbl), path, row.names = FALSE, fileEncoding = "UTF-8")
    written <- c(written, path)
  }

  # Parameter record, so results are reproducible
  param_path <- file.path(dir, "00_parameters.txt")
  con <- file(param_path, open = "wt", encoding = "UTF-8")
  on.exit(close(con), add = TRUE)
  writeLines(c(
    "RobustPanelQCA - analysis record",
    paste("Run at        :", format(x$run_at, "%Y-%m-%d %H:%M:%S")),
    paste("Package       :", as.character(utils::packageVersion("RobustPanelQCA"))),
    paste("Outcome       :", x$outcome),
    paste("Conditions    :", paste(x$conditions, collapse = ", ")),
    paste("Case id       :", x$id_var),
    paste("Time variable :", x$time_var),
    paste("Calibrated    :", x$calibrated),
    paste("Solution type :", x$solution_type),
    paste("Paths found   :", paste(x$terms, collapse = " + ")),
    "",
    utils::capture.output(print(x$params))
  ), con = con)
  written <- c(written, param_path)

  if (!isTRUE(quiet)) {
    message("Wrote ", length(written), " files to ", normalizePath(dir, winslash = "/"))
  }
  invisible(written)
}


#' Render an HTML Report from a Panel fsQCA Result
#'
#' @description
#' Renders a self-contained HTML report with every table of the analysis and a
#' plain-language reading of each one. Requires the `rmarkdown` and `knitr`
#' packages.
#'
#' @param x A `pfsqca_result` object from [run_pfsqca()].
#' @param output_file Character. Name of the HTML file to create.
#'   Default is "pfsqca_report.html".
#' @param output_dir Character. Folder to write the report into. Default is the
#'   working directory.
#' @param title Character. Title shown at the top of the report.
#' @param quiet Logical. Passed to [rmarkdown::render()].
#'
#' @return Invisibly, the path to the rendered HTML file.
#'
#' @examples
#' \dontrun{
#' res <- run_pfsqca(
#'   example_panel, "entrepreneurship",
#'   c("infrastructure", "knowledge", "finance", "talent")
#' )
#' pfsqca_report(res, output_dir = tempdir())
#' }
#'
#' @export
pfsqca_report <- function(x,
                          output_file = "pfsqca_report.html",
                          output_dir = ".",
                          title = "Panel fsQCA Report",
                          quiet = TRUE) {
  if (!inherits(x, "pfsqca_result")) {
    stop("`x` must be a pfsqca_result object created by run_pfsqca().", call. = FALSE)
  }
  for (pkg in c("rmarkdown", "knitr")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop("Package \"", pkg, "\" is needed to build the report.\n  Install it with: install.packages(\"", pkg, "\")",
           call. = FALSE)
    }
  }

  template <- system.file("report", "pfsqca_report.Rmd", package = "RobustPanelQCA")
  if (!nzchar(template)) {
    stop("Report template not found. Reinstall RobustPanelQCA.", call. = FALSE)
  }

  if (!dir.exists(output_dir)) dir.create(output_dir, recursive = TRUE)

  # Pass the result through a temporary file so the report renders in a clean
  # session without depending on the caller's environment.
  res_path <- tempfile(fileext = ".rds")
  saveRDS(x, res_path)
  on.exit(unlink(res_path), add = TRUE)

  # Render from a copy so the installed template is never written to.
  tmp_rmd <- file.path(tempdir(), "pfsqca_report.Rmd")
  file.copy(template, tmp_rmd, overwrite = TRUE)

  out <- rmarkdown::render(
    input = tmp_rmd,
    output_file = output_file,
    output_dir = normalizePath(output_dir, winslash = "/"),
    params = list(res_path = res_path, title = title),
    envir = new.env(parent = globalenv()),
    quiet = quiet
  )

  message("Report written to ", normalizePath(out, winslash = "/"))
  invisible(out)
}


#' Copy the Ready-to-Edit Analysis Script into Your Project
#'
#' @description
#' Copies a fill-in-the-blanks R script (and, optionally, the example dataset
#' as a CSV) into a folder. You edit the parameter block at the top of the
#' script, run it, and get the console summary, the CSV tables, and the HTML
#' report without writing any code.
#'
#' @param path Character. Folder to copy the files into. Default is the working
#'   directory.
#' @param example_data Logical. If TRUE (default), also copies
#'   `example_panel.csv` so the script runs out of the box.
#' @param overwrite Logical. If FALSE (default), existing files are left alone.
#'
#' @return Invisibly, the character vector of files written.
#'
#' @examples
#' use_pfsqca_template(path = tempdir())
#'
#' @export
use_pfsqca_template <- function(path = ".", example_data = TRUE, overwrite = FALSE) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)

  sources <- c(system.file("templates", "run_analysis.R", package = "RobustPanelQCA"))
  targets <- c(file.path(path, "run_analysis.R"))

  if (isTRUE(example_data)) {
    sources <- c(sources, system.file("extdata", "example_panel.csv", package = "RobustPanelQCA"))
    targets <- c(targets, file.path(path, "example_panel.csv"))
  }

  written <- character(0)
  for (i in seq_along(sources)) {
    if (!nzchar(sources[i])) next
    if (file.exists(targets[i]) && !isTRUE(overwrite)) {
      message("Skipping ", basename(targets[i]), " (already exists; use overwrite = TRUE to replace)")
      next
    }
    file.copy(sources[i], targets[i], overwrite = TRUE)
    written <- c(written, targets[i])
  }

  if (length(written) > 0) {
    message("Created:\n  ", paste(normalizePath(written, winslash = "/"), collapse = "\n  "),
            "\n\nNext step: open run_analysis.R, edit the SETTINGS block at the top, and run the whole file.")
  }
  invisible(written)
}
