---
title: "Kernel Regression Smoother"
---
# This script contains implementations of the following:
# - Univariate kernel regression smoothing
# - Bivariate kernel regression smoothing
# - Backfitting algorithm implementation for additive modeling
# - Leave-one-out cross-validation scheme for selecting optimal bandwidth for kernel smoothers
  



# Univariate kernel regression smoothing
Kernel.Reg <- function(data, h) {
  grid <- seq(min(data[,1]), max(data[,1]), len=100)
  x <- data[,1]
  y <- data[,2]
  
  prediction <- c()
  for (i in 1:length(grid)) {
    prediction[i] <- sum(y * (dnorm((x - grid[i]) / h))) / sum(dnorm((x - grid[i] / h)))
  }
  
  return (list(x=grid, y=prediction))
}

# testing
xkcd <- read.csv("xkcd.csv")
model <- Kernel.Reg(xkcd, 0.5)
plot(model$x, model$y, col='blue')
points(xkcd)



"
KERNEL SMOOTHING 
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

ADDITIVE MODELING
One solution for the curse of dimensionality is to assume our model is an \textbf{additive model}, meaning it is of the form:
$$ y = \beta_0 + \sum f_i(x_i) + \epsilon$$ 

We can compress high dimensional data into a unidimensional form using the \textbf{backfitting algorithm}. The backfitting algorithm 
allows you to use a very smart learner like kernel regression only in one dimension at a time (so long as you are running it under 
the assumption that the target function is additive), which avoids some of the issues that come with high dimensionality.  
"