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

"
DENSITY
We want to create a function to where we can select any subinterval that we may need for our distribution. We will thus call this function 
the density function $f(x)$, where $P(a < X < b) = \int_{a}^{b} f(x) dx$. Because the density function outputs the probability that a 
given value lies within that interval, then when we integrate our entire function $f(x)$ then it must equal 1. 

INDICATOR FUNCTION DENSITY ESTIMATION
Perhaps the most rudimentary (but non-trivial) method of measuring density takes inspiration from the histogram. We like the histogram
because of how it is able to predict that if there are a lot of nearby values in one location, then the density will be high, and if there 
are few values in another location, then the density at that location will be low. Drawing from this, we can construct the following 
estimator: 
$$ \hat{f}(x) = \frac{1}{nh} \sum \mathbf{1} (|x_i - x| < \frac{h}{2}) $$

The function above looks at a given value $x$, and checks to see how many data points $x_i$ are nearby (nearby being defined as less than 
our value $h$, which we can call our bandwidth or tolerance). The more values within the range of h, the higher the density at that point, 
and the less values in the range of h, the lower the densty.

KERNEL DENSITY ESTIMATION
The indicator function is not bad, but suffers from a lack of 'smoothness' that tends to follow most real-world data distributions. We 
will introduce the idea of a kernel. The last model we used was currently using is called a 'box kernel' - if we are testing a particular
value, then our current box kernel simply returns 1 for each data point within the box and 0 for each data point out of the box. It does NOT
take into account how close or far away each datapoint is from the value we are testing. 

So instead of simply limiting ourselves to our box kernel, we will expand this to any kernel function $K$ in our density estimator: 
$$ \hat{f}_h(x) = \frac{1}{nh} \sum K (\frac{x_i - x}{h}) $$

For a kernel function to be valid, it must satisfy two things: one, $ \int K(u)du $ must equal 1 for our resulting function to be a density. 
It must also be unimodal and symmetrical with a mean of 0. This allows us to input, say, the normal distribution, which penalizes each 
datapoint in proportion to how far away it is from the value we are testing. 

However we are still left with the same fundamental problem: how do we choose a good $h$?

CROSS-VALIDATION
Using the indicator function or maximum likelihood estimator to estimate data isn't completely useless. The reason it isn't helpful is 
because it predicts our known data well, but says that future data is impossible. But instead what if we could tweak it to maximize future
data, and make a 'maximum future likelihood estimator' function?
$$L_f(x) = \prod f(x^*_j)$$ 

Well, unfortunately knowing future data is impossible, otherwise we wouldn't be doing statistics. But instead, we can do something clever, 
and 'pretend' we have future data by intentionally withholding some data from our model. This is called splitting our training data into 
test data. Then we can run our maximum likelihood estimator on our 'known data', and see how well it predicts our 'test data' or 
'future data'. This process of using part of our known data to find optimal parameters is called cross validation.

One such method of doing cross-validation is called the leave-one-out method. It works as follows: for every value of $h$, and for every 
value of $i$ (where $i$ is the number of datapoints in $x$), obtain the dataset $\{x_j\}, j \neq i$ (ie, dataset $x$ with every value 
except $i$), then compute the maximum likelihood at that point which we may label $f_{-i}(x_i)$. Your output is the specified $h$ that 
maximizes $\prod f_{-i}(x_i)$.
"