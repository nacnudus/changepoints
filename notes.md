# Notes

## [ecp vignette](https://cran.r-project.org/web/packages/ecp/vignettes/ecp.pdf)

Reviews several other packages.

* Parametric (penalisation, no. of changepoints) vs non-parametric
* Types of change (mean, variance, general)
* Types of series (univariate, multivariate)
* Computational cost (quadratic, linear)
* Online vs offline
* Distributions (dependent, independent, gaussian)

The ecp package offers two methods.

* A binary search (with quadratic running time) that apparently gives strongly
  consistent changepoints.
* An agglomerative search (also quadratic but doesn't depend on no. of estimated
  changepoints) that starts with small bits and aggregates them if a
  goodness-of-fit statistic says they're basically the same.

## [Another paper by ecp authors](https://arxiv.org/pdf/1306.4933.pdf)

Offline, not online.  But I am using it online.  Claims that the ecp binary
search leads to consistent changepoints, but only shows that theoretically.

# Univariate vs Multivariate

In the context of the London Underground, perhaps multivariate changepoints (all
lines, one metric) should be considered first, by senior management, before
univariate ones (individual lines) are considered by line management.
