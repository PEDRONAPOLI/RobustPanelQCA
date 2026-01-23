# RobustPanelQCA

<!-- badges: start -->
[![R-CMD-check](https://github.com/PEDRONAPOLI/RobustPanelQCA/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/PEDRONAPOLI/RobustPanelQCA/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

RobustPanelQCA implements panel fuzzy-set Qualitative Comparative Analysis (fsQCA) with comprehensive robustness testing. It provides pooled (POCONS), between-period (BECONS), and within-case (WICONS) consistency metrics for evaluating configurational stability across time and units.

## Features

- **Panel metrics**: POCONS, BECONS, WICONS for sufficiency and necessity
- **Robustness testing**: Random deletion approach (Emmenegger, Schraff & Walter)
- **Calibration**: Percentile-based fuzzy-set calibration
- **Complete workflow**: From calibration to solution analysis

## Installation

You can install and run the development version of RobustPanelQCA from GitHub:
```r
# install.packages("pak")
pak::pak("PEDRONAPOLI/RobustPanelQCA")
library(RobustPanelQCA)
```

## Basic Usage
```r
library(RobustPanelQCA)

# Set parameters
params <- pfsqca_params(incl_cut = 0.75, nec_threshold = 0.90)

# Calibrate data
data_cal <- calibrate_panel(data, vars = c("cond1", "cond2", "outcome"))

# Necessity analysis
nec <- necessity_test(data_cal, "outcome", c("cond1", "cond2"), params)

# Sufficiency analysis
suf <- sufficiency_analysis(data_cal, "outcome", c("cond1", "cond2"), params)

# Panel metrics
pm <- panel_metrics(data_cal, "outcome", suf$terms)

# Robustness testing
rob <- sufficiency_robustness(data_cal, "outcome", c("cond1", "cond2"), params)
```

## Citation

If you use this package, please cite:
```
Napoli, P. H. (2025). RobustPanelQCA: Robust Panel Fuzzy-Set Qualitative 
Comparative Analysis. R package version 0.0.0.9000.
https://github.com/PEDRONAPOLI/RobustPanelQCA
```

## License

GPL-3
