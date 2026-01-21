## Code to prepare `example_panel` dataset

set.seed(42)

n_cases <- 30
n_periods <- 3

example_panel <- data.frame(
  case_id = rep(paste0("case_", sprintf("%02d", 1:n_cases)), each = n_periods),
  period = rep(1:n_periods, times = n_cases),

  # Conditions (raw values before calibration)
  infrastructure = round(runif(n_cases * n_periods, 10, 100), 1),
  knowledge = round(runif(n_cases * n_periods, 5, 80), 1),
  finance = round(runif(n_cases * n_periods, 20, 90), 1),
  talent = round(runif(n_cases * n_periods, 15, 85), 1),

  # Outcome
  entrepreneurship = round(runif(n_cases * n_periods, 0, 100), 1)
)

# Add some structure: higher conditions tend to have higher outcome
example_panel$entrepreneurship <- with(example_panel,
                                       round(0.3 * infrastructure + 0.25 * knowledge + 0.2 * finance + 0.25 * talent +
                                               rnorm(n_cases * n_periods, 0, 10), 1)
)
example_panel$entrepreneurship <- pmax(0, pmin(100, example_panel$entrepreneurship))

usethis::use_data(example_panel, overwrite = TRUE)
