# =============================================================================
#  ROBUST PANEL fsQCA - ANALYSIS SCRIPT
# =============================================================================
#
#  HOW TO USE THIS FILE
#  --------------------
#  1. Edit ONLY the SETTINGS block below (between the two long lines).
#  2. Select everything (Ctrl+A) and run it (Ctrl+Enter), or click "Source".
#  3. Read the summary printed in the console, then open the folder that the
#     script tells you about: it has the CSV tables and an HTML report.
#
#  You do not need to write any R code. If something is wrong, the script
#  stops and tells you in plain language what to fix.
#
#  YOUR DATA MUST BE IN LONG PANEL FORMAT: one row per case per period.
#
#     case_id  period  infrastructure  knowledge  finance  talent  entrepreneurship
#     case_01       1            92.3       55.1     84.3    24.5              62.7
#     case_01       2            94.3        5.0     80.4    70.0              68.3
#     case_01       3            35.8       20.6     42.2    46.7              33.7
#     case_02       1            84.7       75.0     38.1    24.5              51.3
#     ...
#
#  Open example_panel.csv (copied next to this script) to see the exact layout.
# =============================================================================


# vvvvvvvvvvvvvvvvvvvvvvvvvv  SETTINGS - EDIT BELOW  vvvvvvvvvvvvvvvvvvvvvvvvvv

## ---- 1. WHERE IS YOUR DATA? ----
# Put the file in the same folder as this script and write its name here.
# Accepted formats: .csv, .xlsx, .xls, .rds
# Leave it as "example_panel.csv" to try the script with the example dataset.
DATA_FILE <- "example_panel.csv"

# If your CSV uses ";" as separator and "," as decimal mark (common in
# Brazil and much of Europe), set this to TRUE.
CSV_SEMICOLON <- FALSE


## ---- 2. WHICH COLUMNS ARE WHAT? ----
# Write the column names EXACTLY as they appear in your file (R is
# case-sensitive: "Period" is not the same as "period").

ID_COLUMN     <- "case_id"           # identifies the case (firm, city, country...)
TIME_COLUMN   <- "period"            # identifies the time period (year, wave...)
OUTCOME       <- "entrepreneurship"  # the result you want to explain

CONDITIONS <- c(                     # the explanatory conditions (2 to 6 works best)
  "infrastructure",
  "knowledge",
  "finance",
  "talent"
)


## ---- 3. CALIBRATION ----
# fsQCA needs values between 0 and 1 ("fuzzy sets"). If your data holds raw
# values (scores, R$, %, counts), leave CALIBRATE as TRUE and the script
# converts them using percentiles.
CALIBRATE <- TRUE

# Percentiles used as anchors: full exclusion / crossover / full inclusion.
# 0.10 / 0.50 / 0.90 is the common default.
CALIBRATION_PERCENTILES <- c(0.10, 0.50, 0.90)


## ---- 4. ANALYSIS THRESHOLDS ----
INCL_CUT      <- 0.80   # minimum consistency for a truth table row (0.75-0.85 typical)
PRI_CUT       <- 0.50   # minimum PRI (proportional reduction in inconsistency)
N_CUT         <- 1      # minimum number of cases a configuration needs
NEC_THRESHOLD <- 0.90   # a condition is "necessary" above this consistency


## ---- 5. ROBUSTNESS TEST ----
# Randomly deletes part of the cases many times and re-runs the analysis, to
# see which findings survive (Emmenegger, Schraff & Walter).
RUN_ROBUSTNESS   <- TRUE
ROBUSTNESS_RUNS  <- 999   # use 100 for a quick trial, 999 for final results
ROBUSTNESS_DROP  <- 0.10  # share of cases dropped in each run (0.10 = 10%)
ROBUSTNESS_SEED  <- 123   # any number; keeps results reproducible


## ---- 6. DIRECTIONAL EXPECTATIONS (OPTIONAL) ----
# Theory about how each condition should contribute to the outcome. This makes
# the package also compute the INTERMEDIATE solution and separate core from
# contributing conditions.
#   1  = you expect its PRESENCE to contribute
#   0  = you expect its ABSENCE to contribute
#  -1  = no expectation
# Leave as NULL to skip this and report the parsimonious solution only.
DIRECTIONAL_EXPECTATIONS <- NULL
# Example:
# DIRECTIONAL_EXPECTATIONS <- c(
#   infrastructure = 1,
#   knowledge      = 1,
#   finance        = 1,
#   talent         = 1
# )


## ---- 7. OUTPUT ----
RESULTS_FOLDER <- "results"   # folder created next to this script
MAKE_HTML_REPORT <- TRUE      # set to FALSE to only produce CSV files

# ^^^^^^^^^^^^^^^^^^^^^^^^^^  SETTINGS - EDIT ABOVE  ^^^^^^^^^^^^^^^^^^^^^^^^^^




# =============================================================================
#  YOU DO NOT NEED TO CHANGE ANYTHING BELOW THIS LINE
# =============================================================================

message("\n=== RobustPanelQCA ===\n")

## --- Check that the package is installed -------------------------------------
if (!requireNamespace("RobustPanelQCA", quietly = TRUE)) {
  stop(
    "The RobustPanelQCA package is not installed.\n",
    "Run these two lines in the console first:\n\n",
    '  install.packages("pak")\n',
    '  pak::pak("PEDRONAPOLI/RobustPanelQCA")\n',
    call. = FALSE
  )
}
library(RobustPanelQCA)

## --- Read the data -----------------------------------------------------------
read_any <- function(file) {
  if (!file.exists(file)) {
    stop(
      "File not found: ", file, "\n",
      "  Looking in: ", normalizePath(dirname(file), winslash = "/", mustWork = FALSE), "\n",
      "  Put the data file in that folder, or write the full path in DATA_FILE.\n",
      "  Tip in RStudio: Session > Set Working Directory > To Source File Location.",
      call. = FALSE
    )
  }
  ext <- tolower(tools::file_ext(file))
  if (ext == "csv") {
    if (isTRUE(CSV_SEMICOLON)) {
      utils::read.csv2(file, stringsAsFactors = FALSE)
    } else {
      utils::read.csv(file, stringsAsFactors = FALSE)
    }
  } else if (ext %in% c("xlsx", "xls")) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop('Reading Excel files needs the readxl package.\n  Run: install.packages("readxl")',
           call. = FALSE)
    }
    as.data.frame(readxl::read_excel(file))
  } else if (ext == "rds") {
    as.data.frame(readRDS(file))
  } else {
    stop("Unsupported file type: .", ext,
         "\n  Use .csv, .xlsx or .rds.", call. = FALSE)
  }
}

message("Reading ", DATA_FILE, " ...")
dados <- read_any(DATA_FILE)
message("  ", nrow(dados), " rows, ", ncol(dados), " columns.\n")

## --- Assemble the parameters -------------------------------------------------
params <- pfsqca_params(
  incl_cut           = INCL_CUT,
  pri_cut            = PRI_CUT,
  n_cut              = N_CUT,
  nec_threshold      = NEC_THRESHOLD,
  robustness_B       = ROBUSTNESS_RUNS,
  robustness_drop    = ROBUSTNESS_DROP,
  robustness_seed    = ROBUSTNESS_SEED,
  robust_PI_freq_cut = 0.80,
  robust_nec_cut     = 0.80,
  dir_exp            = DIRECTIONAL_EXPECTATIONS
)

## --- Run the whole analysis --------------------------------------------------
resultado <- run_pfsqca(
  data       = dados,
  outcome    = OUTCOME,
  conditions = CONDITIONS,
  id_var     = ID_COLUMN,
  time_var   = TIME_COLUMN,
  params     = params,
  calibrate  = CALIBRATE,
  probs      = CALIBRATION_PERCENTILES,
  robustness = RUN_ROBUSTNESS
)

## --- Show the summary in the console -----------------------------------------
cat("\n\n")
print(resultado)

## --- Save the tables ---------------------------------------------------------
cat("\n")
export_pfsqca(resultado, dir = RESULTS_FOLDER)

## --- Build the HTML report ---------------------------------------------------
if (isTRUE(MAKE_HTML_REPORT)) {
  pfsqca_report(resultado, output_dir = RESULTS_FOLDER)
}

message("\nAll done. Open the \"", RESULTS_FOLDER, "\" folder to see the results.")

# The full result stays in memory as `resultado`, in case you want to dig in:
#   resultado$necessity              # necessity table
#   resultado$coverage               # one row per sufficient path
#   resultado$panel_metrics$pooled   # POCONS / POCOV
#   resultado$panel_metrics$between  # BECONS by period
#   resultado$panel_metrics$within   # WICONS by case
#   resultado$data_cal               # the calibrated data
