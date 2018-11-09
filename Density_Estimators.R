---
title: "Density Estimator"
---
# Univariate density estimator using indicator function kernel
Density.Estimate <- function(x, h, grid) {
  f <- c()
  n <- length(x)
  for (i in 1:length(grid)) {
    y <- grid[i]
    f[i] = (1 / (n*h)) * sum (abs (y-x) < (h/2) )
  }
  plot(grid, f)  
}

# Function testing
set.seed(47)
x <- rnorm(100)
grid <- seq(-4, 4, .01)
h = 3.5

Density.Estimate(x, h, grid)



