% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summary.cv.glmnet.all.R
\name{summary.cv.glmnet.all}
\alias{summary.cv.glmnet.all}
\title{Print a summary of the results of cross-validation for a glmnet model.}
\usage{
\method{summary}{cv.glmnet.all}(object, ...)
}
\arguments{
\item{object}{fitted 'cv.glmnet.all' object}

\item{\dots}{Not used. Other arguments for summary.}
}
\description{
Print a summary of the results of cross-validation for a glmnet model.
}
\examples{
\dontrun{
data("leukemia", package = "varbvs")
X <- leukemia$x
y <- leukemia$y
mod <- cv.glmnet.all(x = X, y = y, family = "binomial", type.measure = "deviance")
summary(mod)
}
}
