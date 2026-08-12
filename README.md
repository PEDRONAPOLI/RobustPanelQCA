# RobustPanelQCA

<!-- badges: start -->
[![R-CMD-check](https://github.com/PEDRONAPOLI/RobustPanelQCA/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PEDRONAPOLI/RobustPanelQCA/actions/workflows/R-CMD-check.yaml)
[![License: GPL-3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

Panel fuzzy-set Qualitative Comparative Analysis (fsQCA) with robustness
testing, in R.

Standard fsQCA treats every observation as a separate case. When your data is a
**panel** — the same cases observed over several periods — that hides two
questions you should be asking: does the solution hold in *every period*, and
does it hold for *every case*? This package answers both, and then checks
whether the answers survive the removal of random subsets of your data.

**You do not need to know R to use this package.** See
[Quick start](#quick-start-no-coding-required) below: you edit a settings block
in a script, run it, and get an HTML report.

---

## What it gives you

| | |
|---|---|
| **Pooled metrics** | POCONS / POCOV — consistency and coverage over the whole panel |
| **Between-period metrics** | BECONS / BECOV — computed separately within each period, to expose solutions that only work in certain years |
| **Within-case metrics** | WICONS / WICOV — computed within each case over time, to expose the cases your solution fails to explain |
| **Robustness testing** | Random deletion (Emmenegger, Schraff & Walter): re-runs the whole analysis hundreds of times on random subsamples and reports which findings survive |
| **Necessity and sufficiency** | Full necessity tests, truth table analysis, Boolean minimization, parsimonious and intermediate solutions, core vs. contributing conditions |
| **Calibration** | Percentile-based direct calibration, with automatic handling of tied quantiles |

---

## Installation

You need [R](https://cran.r-project.org/) (4.1 or newer). [RStudio](https://posit.co/download/rstudio-desktop/)
is strongly recommended.

Run this once in the R console:

```r
install.packages("pak")
pak::pak("PEDRONAPOLI/RobustPanelQCA")
```

That installs the package and everything it depends on. If you are asked
whether to install from sources packages that need compilation, answering `No`
is fine.

---

## Quick start (no coding required)

**Step 1.** Create a folder for your project. In R, set the working directory
to it (in RStudio: *Session > Set Working Directory > Choose Directory*), then
run:

```r
library(RobustPanelQCA)
use_pfsqca_template()
```

This drops two files into your folder:

- `run_analysis.R` — the script you will edit
- `example_panel.csv` — example data, so the script works immediately

**Step 2.** Open `run_analysis.R`. Edit only the block marked `SETTINGS`:

```r
DATA_FILE   <- "example_panel.csv"   # your .csv, .xlsx or .rds file
ID_COLUMN   <- "case_id"             # column identifying the case
TIME_COLUMN <- "period"              # column identifying the period
OUTCOME     <- "entrepreneurship"    # what you want to explain

CONDITIONS  <- c("infrastructure", "knowledge", "finance", "talent")

INCL_CUT    <- 0.80                  # consistency cutoff
N_CUT       <- 1                     # minimum cases per configuration
```

**Step 3.** Run the whole file (*Source*, or Ctrl+Shift+S). You get a summary in
the console and a `results/` folder containing an HTML report plus one CSV per
table.

That is the entire workflow. Everything below is for when you want more control.

### How your data must be laid out

One row per case per period, one column per condition — "long" format:

| case_id | period | infrastructure | knowledge | finance | talent | entrepreneurship |
|---------|--------|----------------|-----------|---------|--------|------------------|
| case_01 | 1      | 92.3           | 55.1      | 84.3    | 24.5   | 62.7             |
| case_01 | 2      | 94.3           | 5.0       | 80.4    | 70.0   | 68.3             |
| case_01 | 3      | 35.8           | 20.6      | 42.2    | 46.7   | 33.7             |
| case_02 | 1      | 84.7           | 75.0      | 38.1    | 24.5   | 51.3             |

Values can be on any scale (scores, currency, percentages, counts) — the
package calibrates them into fuzzy sets for you. Column names are up to you;
you declare them in the settings block.

---

## Using it from R directly

The whole analysis in one call:

```r
library(RobustPanelQCA)

res <- run_pfsqca(
  data       = example_panel,
  outcome    = "entrepreneurship",
  conditions = c("infrastructure", "knowledge", "finance", "talent"),
  id_var     = "case_id",
  time_var   = "period",
  params     = pfsqca_params(incl_cut = 0.80, n_cut = 1)
)

res                   # printed summary
export_pfsqca(res)    # write every table to CSV
pfsqca_report(res)    # render the HTML report
```

Or step by step, if you need to justify each decision:

```r
conditions <- c("infrastructure", "knowledge", "finance", "talent")
params     <- pfsqca_params(incl_cut = 0.80, n_cut = 1)

data_cal <- calibrate_panel(example_panel, vars = c(conditions, "entrepreneurship"))

necessity_test(data_cal, "entrepreneurship", conditions, params)
suf <- sufficiency_analysis(data_cal, "entrepreneurship", conditions, params)
suf$terms
#> "infrastructure*knowledge"  "finance*talent"

panel_metrics(data_cal, "entrepreneurship", suf$terms)
unique_coverage(data_cal, "entrepreneurship", suf$terms)
sufficiency_robustness(data_cal, "entrepreneurship", conditions, params)
```

For the full walkthrough with commentary:

```r
vignette("panel-fsqca-workflow", package = "RobustPanelQCA")
```

And for a plain-language explanation of what every number means, read
[GUIDE.md](GUIDE.md).

---

## Function reference

**Main entry points**

| Function | What it does |
|---|---|
| `use_pfsqca_template()` | Copies the ready-to-edit script into your project |
| `run_pfsqca()` | Runs the complete analysis |
| `export_pfsqca()` | Writes every result table to CSV |
| `pfsqca_report()` | Renders the HTML report |
| `pfsqca_params()` | Sets all thresholds in one object |

**Individual steps**

| Function | What it does |
|---|---|
| `calibrate_panel()`, `calibrate_percentile()` | Percentile calibration into fuzzy sets |
| `necessity_test()` | Necessity of each condition and its negation |
| `necessity_panel_diagnostics()` | Necessity within each period and each case |
| `necessity_robustness()` | Necessity under random deletion |
| `sufficiency_analysis()` | Truth table, minimization, core vs. contributing |
| `panel_metrics()` | POCONS / BECONS / WICONS |
| `unique_coverage()` | Raw and unique coverage per path |
| `sufficiency_robustness()` | Sufficient paths under random deletion |
| `create_ball_table()` | Solution table with the usual circle notation |
| `literal_membership()`, `term_membership()`, `solution_membership()` | Fuzzy-set algebra on terms |
| `cons_cov_suf()`, `cons_cov_nec()` | The underlying consistency and coverage formulas |

---

## Example dataset

`example_panel` is fictional: 30 regional innovation ecosystems over 3 periods.
It was generated with two genuine sufficient paths to a high outcome —
`infrastructure * knowledge` and `finance * talent` — and no necessary
condition. A correct analysis recovers exactly that, which makes it useful for
checking that you are reading the output right.

```r
data(example_panel)
head(example_panel)
```

The generating code is in [`data-raw/example_panel.R`](data-raw/example_panel.R).

---

## Citation

```
Napoli, P. H. (2026). RobustPanelQCA: Robust Panel Fuzzy-Set Qualitative
Comparative Analysis. R package version 0.1.0.
https://github.com/PEDRONAPOLI/RobustPanelQCA
```

Methods implemented here build on:

- Ragin, C. C. (2008). *Redesigning Social Inquiry: Fuzzy Sets and Beyond*. University of Chicago Press.
- Emmenegger, P., Schraff, D., & Walter, A. (2014). QCA, the Truth Table Analysis and Large-N Survey Data. *COMPASSS Working Paper* 2014-79.
- Schneider, C. Q., & Wagemann, C. (2012). *Set-Theoretic Methods for the Social Sciences*. Cambridge University Press.
- Dusa, A. (2019). *QCA with R: A Comprehensive Resource*. Springer.

## Problems?

Open an issue at
[github.com/PEDRONAPOLI/RobustPanelQCA/issues](https://github.com/PEDRONAPOLI/RobustPanelQCA/issues),
including the error message and the code you ran.

## License

GPL-3
