## Code to prepare the `example_panel` dataset
##
## This is FICTIONAL data, generated so that it teaches the two ideas fsQCA
## exists to detect:
##
##   1. Equifinality  - two different configurations lead to the same outcome.
##   2. Conjunctural causation - conditions matter in combination, not alone.
##
## The data-generating process has two sufficient paths to high
## entrepreneurship:
##
##   Path A: infrastructure AND knowledge   (technology-driven ecosystems)
##   Path B: finance        AND talent      (capital + people driven ecosystems)
##
## Neither path is necessary (either one alone is enough), so a correct
## analysis should find NO necessary condition and TWO sufficient paths.
##
## Cases also carry a case-level random effect, so a case that scores high in
## period 1 tends to score high in periods 2 and 3. That persistence is what
## makes the within-case (WICONS) metrics meaningful.

set.seed(2024)

n_cases   <- 30
n_periods <- 3
n_obs     <- n_cases * n_periods

case_id <- rep(paste0("case_", sprintf("%02d", seq_len(n_cases))), each = n_periods)
period  <- rep(seq_len(n_periods), times = n_cases)

# Latent case-level strength on each condition (persistent over time),
# plus period-specific shocks.
draw_condition <- function(case_sd = 1.0, shock_sd = 0.45) {
  case_level <- rep(stats::rnorm(n_cases, 0, case_sd), each = n_periods)
  shock      <- stats::rnorm(n_obs, 0, shock_sd)
  case_level + shock
}

latent <- data.frame(
  infrastructure = draw_condition(),
  knowledge      = draw_condition(),
  finance        = draw_condition(),
  talent         = draw_condition()
)

# Observed 0-100 scores
to_score <- function(z) round(100 * stats::plogis(z), 1)
raw <- as.data.frame(lapply(latent, to_score))

# Fuzzy-set membership implied by the latent scores (used only to build the
# outcome; the analyst re-derives this through calibration).
to_fuzzy <- function(v) (rank(v, ties.method = "average") - 0.5) / length(v)
fz <- as.data.frame(lapply(raw, to_fuzzy))

path_a <- pmin(fz$infrastructure, fz$knowledge)  # infra AND knowledge
path_b <- pmin(fz$finance, fz$talent)            # finance AND talent
sufficient <- pmax(path_a, path_b)               # either path is enough

# The outcome sits at or above the membership in the sufficient paths, which is
# what makes those paths consistent subsets of the outcome.
y_fuzzy <- 0.88 * sufficient + 0.10 + stats::rnorm(n_obs, 0, 0.05)
y_fuzzy <- pmin(1, pmax(0, y_fuzzy))

example_panel <- data.frame(
  case_id          = case_id,
  period           = period,
  infrastructure   = raw$infrastructure,
  knowledge        = raw$knowledge,
  finance          = raw$finance,
  talent           = raw$talent,
  entrepreneurship = round(100 * y_fuzzy, 1),
  stringsAsFactors = FALSE
)

usethis::use_data(example_panel, overwrite = TRUE)

# Also ship a plain CSV so users can see the exact input format the package
# expects (one row per case-period, one column per condition).
utils::write.csv(
  example_panel,
  file.path("inst", "extdata", "example_panel.csv"),
  row.names = FALSE
)
