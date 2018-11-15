---
title: "Kernel Regression Smoother"
---
# This script contains implementations of the following:
# - Univariate kernel regression smoothing
# - Bivariate kernel regression smoothing
# - Backfitting algorithm implementation for additive modeling
# - Leave-one-out cross-validation scheme for selecting optimal bandwidth for kernel smoothers
  
  
# Univariate density estimator, with indicator function kernel
Predict.Y <- function(x, h) {
  for (i in 1:(nrow(data) - 1)) {
    sum(data[,2] * dnorm(x - data[i,1] / h)) / sum(dnorm((x - data[i,1]) / h))
  }
}

# Demonstration for above
h_grid <- seq(0, 1, 0.01)


# Leave-one-out cross-validation scheme
Leave.One.Out <- function(x) {
  return (TRUE)
}
