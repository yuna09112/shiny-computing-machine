#Problem 1

#krr_fit.R
#' @title Gaussian Kernel Matrix
#' @description Calculates the Gaussian Kernel matrix between two sets of data.
#' K(x, x') = exp(-rho * ||x - x'||^2)
#' @param x A matrix of features (n1 x p).
#' @param x_prime A matrix of features (n2 x p).
#' @param rho The Gaussian kernel bandwidth parameter (default is 1).
#' @return A kernel matrix K (n1 x n2).
#' @export
gaussian_kernel <- function(x, x_prime, rho = 1) {
  # Assignment 1's implementation focuses on the core calculation
  n1 <- nrow(x) [cite: 417]
  n2 <- nrow(x_prime) [cite: 418]
  K <- matrix(0, nrow = n1, ncol = n2) [cite: 420]
  
  # Calculate K[i, j] = exp(-rho * ||x[i,] - x_prime[j,]||^2)
  for (i in 1:n1) { [cite: 421]
    for (j in 1:n2) { [cite: 422]
      diff <- sum((x[i,] - x_prime[j,])^2) [cite: 424]
      K[i, j] <- exp(-rho * diff) [cite: 425]
    }
  }
  return(K) [cite: 428]
}

# Problem1 - (a)
#' @title Fit Kernel Ridge Regression Model
#' @description Fits a Kernel Ridge Regression (KRR) model and calculates the dual coefficients (alpha).
#' @param X A matrix of training features.
#' @param y A vector of training responses.
#' @param lambda The regularization (penalty) parameter (default is 0.0001). 
#' @param rho The Gaussian kernel bandwidth parameter (default is 1). 
#' @return An object of class "krr" containing X, y, alpha, lambda, and rho. [cite: 441, 443]
#' @export
krr <- function(X, y, lambda = 0.0001, rho = 1) {
  # Calculate Kernel Matrix K
  K <- gaussian_kernel(X, X, rho) [cite: 433, 434]
  n <- nrow(K) [cite: 437]
  
  # Calculate alpha: alpha = (K + lambda * diag(n)) %*% y
  alpha <- solve(K + lambda * diag(n)) %*% y [cite: 438, 440]
  
  # Create model object
  model <- list(X = X, y = y, alpha = alpha, lambda = lambda, rho = rho) [cite: 441, 446]
  class(model) <- "krr" [cite: 443]
  return(model) [cite: 444]
}


# (c)
library(devtools)

devtools::document()

# (d)
# krr.pkg: Kernel Ridge Regression R Package

## Overview

This package implements the Kernel Ridge Regression (KRR) algorithm using a Gaussian Kernel, based on the statistical computing implementation in Assignment 1.

## Installation

(Instructions for installing the package from GitHub)

## Usage Example

### 1. Simulate Data

We simulate a simple non-linear regression problem based on a sine and cosine function with noise.

```R
set.seed(1)
n <- 150
# Generate X uniformly
X <- matrix(runif(n, 0, 1), ncol = 1) 
# True function f(x) = sin(2*pi*x) + 0.5*cos(4*pi*x)
ftrue <- function(x) sin(2*pi*x) + 0.5*cos(4*pi*x) 
# Add noise to get y
y <- ftrue(X[,1]) + rnorm(n, sd = 0.1)

# Load the package (once installed)
# library(krr.pkg) 

# Fit the model
model <- krr(X, y, lambda = 0.001, rho = 5)

# Predict on a sequence (e.g., for plotting)
X_test <- matrix(seq(min(X), max(X), length.out = 200), ncol = 1)
y_predicted <- predict(model, X_test) 

# Visualization (Assumes plot.krr is defined and exported)
plot(model, ftrue)

# (e)
Package: krr.pkg
Title: KRR Implementation
Version: 0.0.1
Authors@R: person("Yuna", "Park", email = "20220880@sungshin.ac.kr", role = c("aut", "cre"))
Description: A simple R package for fitting and predicting using the
Kernel Ridge Regression algorithm with a Gaussian kernel.
License: GPL-3
Encoding: UTF-8
Roxygen: list(markdown = TRUE)