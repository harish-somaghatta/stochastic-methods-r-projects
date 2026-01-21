# Stochastic Methods (R) — Applied Statistics Projects

This repository contains **applied statistics mini-projects in R**, including:

- Normality tests (Shapiro–Wilk)
- Variance tests (F-test)
- Hypothesis testing (Welch t-test, paired t-test)
- Correlation analysis (Pearson correlation test)
- Confidence intervals
- Regression diagnostics + model selection
- Nonlinear regression (Rodbard function using `nls`)

---

## Repository Structure

```text
.
├── 01_cd_contamination_hypothesis_testing.R
├── 02_paired_ttest_and_correlation.R
├── 03_empirical_distribution_and_confidence_interval.R
├── 04_regression_diagnostics_and_forward_selection.R
├── 05_concentration_decay_nonlinear_regression.R
├── README.md
├── cherry.csv
├── coal_data.txt
├── saet.csv
└── tasks_stochastic_methods.pdf
```

## How to Run
Open R / RStudio in this folder and run:
```r
source("01_cd_contamination_hypothesis_testing.R")
source("02_paired_ttest_and_correlation.R")
source("03_empirical_distribution_and_confidence_interval.R")
source("04_regression_diagnostics_and_forward_selection.R")
source("05_concentration_decay_nonlinear_regression.R")
```
## Scripts Overview
1. Cd contamination comparison (Location A vs Location B)
 - **File**: 01_cd_contamination_hypothesis_testing.R
 - Parallel boxplots + five-number summary
 - Shapiro–Wilk normality test (both samples)
 - F-test for equality of variances
 - Welch t-test to test if mean(Location A) > mean(Location B)
2. Drug effect study (paired sample)
 - **File**: 02_paired_ttest_and_correlation.R
 - Paired sample situation (before vs after)
 - Scatter plot + correlation estimate
 - Pearson correlation test
 - Paired t-test + normality check of differences
3. Coal mine disaster intervals
 - **File**: 03_empirical_distribution_and_confidence_interval.R
 - **Data**: coal_data.txt
 - Histogram + kernel density estimate
 - 95% confidence interval for the mean time between disasters
4. Black cherry tree volume modeling (regression + model selection)
 - **File**: 04_regression_diagnostics_and_forward_selection.R
 - **Data**: coal_data.txt
 - Cylinder-based linear model formulation
 - Residual analysis and influence check
 - Model comparison (ANOVA / F-tests)
 - Forward selection for quadratic terms
5. Pollutant decay modeling using Rodbard function (nonlinear regression)
 - **File**: 05_concentration_decay_nonlinear_regression.R
 - **Data**: saet.csv
 - Fit Rodbard model using nls
 - Plot measured data + fitted curve
 - Estimate t0.5 and compute an upper bound at 95% confidence
