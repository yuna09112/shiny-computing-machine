# Problem 2

# (a)
# R function for Gaussian Kernel Matrix
gaussian_kernel_R <- function(X, rho) {
  # Calculate squared Euclidean distance between all pairs
  sq_dist <- as.matrix(dist(X, method = "euclidean"))^2
  
  # Apply the kernel formula
  K <- exp(-rho * sq_dist)
  return(K)
}

# (b)
// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// [[Rcpp::export]]
arma::mat gaussian_kernel_Rcpp(const arma::mat& X, double rho) {
  int n = X.n_rows;
  arma::mat K(n, n);
  double sq_dist;
  
  for (int i = 0; i < n; ++i) {
    for (int j = i; j < n; ++j) {
      // Calculate squared Euclidean distance
      sq_dist = arma::accu(arma::square(X.row(i) - X.row(j)));
      
      // Apply the kernel formula
      K(i, j) = exp(-rho * sq_dist);
      
      // Matrix is symmetric
      K(j, i) = K(i, j);
    }
  }
  return K;
}

# (c)
# R Code for Benchmarking

library(microbenchmark)

# Data Simulation
set.seed(123)
n <- 1000
X <- matrix(runif(n * 5), ncol = 5) # Example: 5 features
rho_val <- 1

# Ensure Rcpp function is sourced and compiled (e.g., using RStudio's Source button)

bench_result <- microbenchmark(
  R_code = gaussian_kernel_R(X, rho_val),
  Rcpp_code = gaussian_kernel_Rcpp(X, rho_val),
  times = 50
)

# Summarize and present results in a table
summary_table <- summary(bench_result)[, c("expr", "min", "mean", "median", "max")]

# Print the comparison table
print(summary_table)

# (d)
# Example Output Structure (Results will vary)
#       expr       min       mean     median        max
# 1   R_code   75.87ms  80.250ms   78.91ms    95.12ms
# 2 Rcpp_code  3.12ms   3.30ms    3.25ms     4.01ms
