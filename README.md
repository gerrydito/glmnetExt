---
title: glmnetExt
output: github_document
---

![R-CMD-check](https://github.com/hongooi73/glmnetUtils/workflows/R-CMD-check/badge.svg)

[glmnet](https://github.com/cran/glmnet) is an efficient package for fitting the entire lasso or elastic-net regularization path for several regression problems.

The `glmnetExt` package provides some  functions to extend `glmnet`, specifically:

* `cv.glmnet.all` provides capability to find both the alpha and lambda parameters via cross-validation, following the approach described in the help page for `cv.glmnet`
* Methods for `plot`, `predict` and `summary` for `cv.glmnet.all`.
* `boot_glmnet` provides capability to Bootstrap Confidence Interval for model's coefficient and importance measure based on this [paper](https://www.sciencedirect.com/science/article/abs/pii/S0950705121003804)



