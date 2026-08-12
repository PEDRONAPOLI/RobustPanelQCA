# RobustPanelQCA 0.1.0

First release intended for use by other people.

## New features

* `run_pfsqca()` runs the complete workflow (calibration, necessity, necessity
  panel diagnostics, sufficiency, panel metrics, coverage, and both robustness
  tests) in a single call, and returns a `pfsqca_result` object with a readable
  `print()` method.
* `use_pfsqca_template()` copies a fill-in-the-blanks analysis script and the
  example data into your project, so an analysis can be run without writing R.
* `export_pfsqca()` writes every result table to CSV, plus a plain-text record
  of the parameters used.
* `pfsqca_report()` renders a self-contained HTML report that explains how to
  read each table.
* `create_ball_table()` is now exported (it was documented but missing from
  `NAMESPACE`).
* Added `GUIDE.md`, a guide to the method and the output for readers with no
  prior QCA or R background.

## Breaking changes

* `panel_metrics()` and `necessity_panel_diagnostics()` now default to
  `id_var = "case_id"` and `time_var = "period"`, matching `example_panel`. The
  previous defaults (`"MSA"` and `"year"`) came from a specific application and
  caused an error on any other dataset.
* Those functions no longer rename the grouping column in their output: the
  column keeps the name you passed in `id_var` / `time_var`. Previously the
  output was always labelled `case_id` and `year` regardless of the input.

## Improvements

* Input validation across all analysis functions. Missing or misspelled columns
  now produce an error that lists the columns actually present, and passing
  uncalibrated data produces a warning that names the offending variables.
* `sufficiency_analysis()` reports which thresholds to adjust when no truth
  table row clears the consistency cutoff, instead of failing inside the QCA
  package.
* `calibrate_panel()` warns when a variable has too little variation to be
  calibrated, rather than silently returning `NA`.
* `example_panel` was regenerated from a data-generating process with two
  genuine sufficient paths (`infrastructure*knowledge` and `finance*talent`)
  and persistent case-level effects. The analysis recovers exactly those two
  paths and no necessary condition, so the dataset can be used to check that
  output is being read correctly. It also ships as
  `inst/extdata/example_panel.csv` to document the expected input layout.
* Documentation examples now run against the example data instead of being
  wrapped in `\dontrun{}`.
* Added a GitHub Actions R-CMD-check workflow (the README badge previously
  pointed at a workflow that did not exist).
