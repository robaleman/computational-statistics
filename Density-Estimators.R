---
title: "Density Estimator"
---
# This script contains implementations of the following:
# - Univariate density estimator, with indicator function kernel.
# - Univariate density estimator, with Gaussian kernel.
# - Leave-one-out cross-validation scheme for selecting optimal bandwidth for either density estimator.
  

# Univariate density estimator, with indicator function kernel
Density.Estimate <- function(x, h) {
  grid <- seq(-4, 4, .001)
  n <- length(x)
  f <- c()
  for (i in 1:length(grid)) {
    x.i <- grid[i]
    f[i] = (1 / (n*h)) * sum (abs(x.i - x) < (h/2))
  }
  return(f)
}

# Demonstration for above
test_x <- rnorm(100)
test_h = 3.5
test_dens_estimate <- Density.Estimate(test_x, test_h)
plot(seq(-4, 4, .001), test_dens_estimate)


# Univariate density estimator, with Gaussian kernel
Density.Estimate <- function(x, h) {
  grid <- seq(-4, 4, .001)
  n <- length(x)
  f <- c()
  for (i in 1:length(grid)) {
    x.i <- grid[i]
    f[i] = sum( dnorm( (x.i - x) / h) ) / (n * h)
  }
  return (f)
}

# Demonstration for above function
test_x <- rnorm(100)
test_h = 3.5
test_dens_estimate <- Density.Estimate(test_x, test_h)
plot(seq(-4, 4, .001), test_dens_estimate)


# Leave-one-out cross-validation scheme for density estimator functions (TBA)
CV.LeaveOneOut <- function(dens) {
  h <- seq(01,.2,.01)
  n <- length(dens)
  pseudo.likelihood.all <- c()
  
  for (i in length(h)) {
    psuedo.likelihood <- 1
    for (j in length(dens)) {
      pseudo.likelihood.all <- Density.Estimate(x[-j], h[i])
    }
    pseudo.likelihood.all[i] <- pseudo.likelihood
  }
}

# Demonstration for above function
LeaveOneOut ( rbeta(100,1,1) )
