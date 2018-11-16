---
title: "Density Estimator"
---
# This script contains implementations of the following:
# - Univariate density estimator, with indicator function kernel.
# - Univariate density estimator, with Gaussian kernel.
# - Leave-one-out cross-validation scheme for selecting optimal bandwidth for either density estimator.
  

# univariate density estimator, with indicator function kernel
Box.Density.Estimate <- function(x, h) {
  grid <- seq(-4, 4, .001)
  n <- length(x)
  f <- c()
  for (i in 1:length(grid)) {
    x.i <- grid[i]
    f[i] = (1 / (n*h)) * sum (abs(x.i - x) < (h/2))
  }
  return(f)
}

# testing
test_x <- rnorm(100)
test_h = 3.5
test_dens_estimate <- Box.Density.Estimate(test_x, test_h)
plot(seq(-4, 4, .001), test_dens_estimate)


# univariate density estimator, with Gaussian kernel
Kernel.Density.Estimate <- function(x, h) {
  grid <- seq(-4, 4, .001)
  n <- length(x)
  f <- c()
  for (i in 1:length(grid)) {
    x.i <- grid[i]
    f[i] = sum( dnorm( (x.i - x) / h) ) / (n * h)
  }
  return (f)
}

# testing
test_x <- rnorm(100)
test_h = 3.5
test_dens_estimate <- Density.Estimate(test_x, test_h)
plot(seq(-4, 4, .001), test_dens_estimate)


# leave-one-out cross-validation routine for density estimator functions (in progress)
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

# testing
LeaveOneOut ( rbeta(100,1,1) )



"
DENSITY
We are often tasked with estimating the distribution of these random variables. The distribution can be loosely defined as a function
that can tell us the relative likelihood that we will find some quantity in a specified interval. These quantities can be either 
discrete values or continuous values. 

Estimating the distribution of discrete values is pretty straightforward: simply find the proportion, or in other words, the 
percentage of times that $k$ appears in a random sample. This will yield us $P(X = k)$. This method is called the probability 
mass function (PMF) and is simply represented as: 
$$\hat{p}(x) = \sum \frac{\mathbf{1}(x_i = k)}{n}$$ 

The symbol $\mathbf{1}$ refers to the \textbf{indicator function}, which simply says to refers to return 1 if the inner equation is 
satisfied and return 0 if the inner equation is false. In other words, sum up the number of times a given quantity $k$ appears and 
divide it by the sample size. 

However, the above strategy cannot be used for continuous distributions. Since there is an infinite number of values on any given 
subsection of the real number line, the probability that any specific value appears more than once when plucked from a continuous 
distribution is effectively zero. In fact, $P(X = k) = 0$ technically for all values $k$ on any part of the real number line. 

Instead when working on any continuous space, we must instead work in terms of intervals. Alternatively, we can think about the 
probablity that a value falls within a certain range $P(a < X < b)$. 

We want to create a function to where we can select any subinterval that we may need for our distribution. We will thus call this 
function the density function $f(x)$, where $P(a < X < b) = \int_{a}^{b} f(x) dx$. Because the density function outputs the probability
that a given value lies within that interval, then when we integrate our entire function $f(x)$ then it must equal 1. 

So if $f(x)$ is our 'perfect' or 'true population' density function, what are some of the ways that we can estimate this function? 
We call an estimate of the true density function the probability density function, or PDF.


INDICATOR FUNCTION DENSITY ESTIMATION
Perhaps the most rudimentary (but non-trivial) method of measuring density takes inspiration from the histogram. We like the 
histogram because of how it is able to predict that if there are a lot of nearby values in one location, then the density will 
be high, and if there are few values in another location, then the density at that location will be low.

More specifically, the histogram works as follows: break up your dataset into intervals called 'bins', and count up the number 
of observations that falls under each bin. If you rescale the dataset so that the sum of the area of all the bins is equal to 1,
then we actually have a fairly decent density estimator: the height of each bin is the density, or the probability that a given 
value falls under that interval.  Drawing from this computational method, we can construct the following estimator: 
$$ \hat{f}(x) = \frac{1}{nh} \sum^i_{i=1} \mathbf{1} (|x_i - x| < \frac{h}{2}) $$

The function does the following: create $i$ bins, where the center of each bin is value $x_i$. For each value of $x_i$, checks to 
see how many data points $x$ are nearby (nearby being defined as less than our value $h$, which we can call our bandwidth or 
tolerance). $h$ is a value that is selected by us - more about how we select an $h$ soon. But ultimately, we end up with a function 
that says the areas with more nearby values, the higher the density, and the areas with less nearby values, the lower the densty. 

A good start, but there are some problems with this indicator function density estimator. One problem is subjectivity: choices such
as number of bins, bin size, and start/stop values of bin boundaries introduce the element of choice which can (intentionally or 
unintentionally) skew the data. The second main problem of the histogram is its lack of 'smoothness'. Most random distributions 
in nature tend to be smooth 'smooth'. There is no precise definition of smoothness, but we can say that is approximately means 
that if we observe a value $x$ in the dataset, then it is reasonable to assume that another value near $y$ exists nearby on 
that function. We can go further and say that a 'smooth' function is one that is continuous, or even one that is differentiable.
No spikes, corners, or jagged edges. 


KERNEL DENSITY ESTIMATION
To solve the lack of smoothness associated with our previous density estimator, we will introduce the idea of a \textbf{kernel}. 
A kernel density estimator works by replacing our sequenced bins with some sort of distribution instead. Note how our last model 
simply returned 1 for data points within the bin and 0 for any data points that were out of the bin. It does NOT take into account
how close or far away each datapoint is from the value we are testing. 

The histogram-method above was technically a type of kernel called the 'box kernel', where the kernel function was the indicator
function. But what if we had a kernel function that was able to return a value between 0 and 1, where the value is higher the 
closer you are to the center of the bin, and lower the further you are from the center. The most common function with this
capability is the normal distribution. So instead of limiting our density estimator to the box kernel, we can generalize our past
model to any kernel function $K$:
$$ \hat{f}_h(x) = \frac{1}{nh} \sum K (\frac{x_i - x}{h}) $$

Note, that for a kernel function to be valid, it must satisfy two assumptions: one, $ \int K(u)du$ must equal 1 for our resulting 
function to be a density. It must also be unimodal and symmetrical with a mean of 0. This allows us to input, say, the normal 
distribution, which penalizes each datapoint in proportion to how far away it is from the value we are testing. 


CROSS-VALIDATION
Maximum likelihood estimation isn't a good method of guessing density because although it predicts our known data well, it has 
zero predictive power with any future data due to the nature of continuous random variables. But it does get outstanding results
with sample data (perfect results, in fact). Is there a way we can harness the power of maximum likelihood on our sample data 
to make anything practical?

Say we could modify our equation so that it is configured to maximize future data, and make a 'maximum future likelihood estimator'
function? If we say $f^*$ is our future data, then what if it was possible to maximize:
$$L_f(x) = \prod f(x^*_j)$$ 

Well, unfortunately knowing future data is impossible, otherwise there would be no purpose to the field of inferential statistics.
Making such a claim even seems pretty silly. But instead, what if we 'pretend' we have future data by intentionally 
withholding some data from our model. This process is called splitting our data into two piles: training data and test data. Then 
we can run our maximum likelihood estimator on our 'known data', and see how well it predicts our 'test data' or 'future data'. 
This process of intentionally withholding some of our known data in order to test our model's performance is called cross 
validation. We use cross validation to find our optimal parameters.

One such method of doing cross-validation is called the leave-one-out method. Given some dataset $x_i$ where $i$ is the number 
of points in $x$, the algorithm works as follows: for every value of $h$, and for every value of $i$, take the subset 
$\{x_j\}, j \neq i$ (ie, $x$ with every value except $i$), then compute the maximum likelihood at that point which we may 
label $f_{-i}(x_i)$. Your output is the $h$ value that maximizes $\prod f_{-i}(x_i)$. 
"