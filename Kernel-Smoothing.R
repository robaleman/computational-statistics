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

"
KERNEL REGRESSION 
One way to create regression models: what if we take the concept we used for a density estimator, and use it as a regression estimator? 
Using similar methodology to KDE, we can use a weighted system to make predictions, where we look at each point, and calculate a score 
using the proximity of nearby points, weighing close points higher and weighing far-away points lower. This weighting methodology is called 
\textbf{LOESS}. This results in a kernel smoothing regression that looks something like this:
$$\hat{f}(x) = \frac { \sum y_i K (\frac{x_i-x}{h}) } { \sum K (\frac{x_i-x}{h}) } $$ 

Kernel regression smoothing works okay in some scenarios, but presents a lot of problems. First problem: similar to KDE, we are required to 
select parameter $h$. If $h$ is way too small, we will overfit, or 'connect the dots' with our points. I $h$ is way too big, then we will 
underfit, and just get a non-helpful straight line. We can at least solve this problem with cross-validation. 
(We also need to select a kernel $K(\dot)$ but this tends to not be super important).

But the bigger problem presented by kernel smoothing is its questionable effectiveness in higher dimensins. Note that we calculate weighting
using 'closeness', and closeness is defined using distance. Distance becomes an increasingly useless metric in higher and higher dimensions.
This is called the \textbf{curse of dimensionality}, in which the distance between points grow further and further apart. This creates a 
'sparse' dataset which has a number of undesirable properties: for example, certain datapoints called 'hubs' become the nearest neighbour 
of a huge percentage of the other points in the dataset. Such a machine learning model used by Pandora for example may keep recommending the
same songs over and over and over again. 

Because of this issue, it's almost always better to either use an additive model with backfitting, or to just choose a different model
in which distance is not used to measure 'closeness'.

"