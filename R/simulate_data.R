#' simulate_data
#'
#' @name simulate_data
#'
#' @description Simulate a dataset. This can optionally include variables with a given associated with the outcome.
#'
#' @param nrows The number of rows to simulate.
#' @param ncols The number of columns to simulate.
#' @param n_true The number of variables truly associated with the outcome.
#' @param amplitude The strength of association between true variables and the outcome.
#'
#' @return A simulated dataset
#'
#' @import dplyr
#'
#' @export

simulate_data <- function(nrows, ncols, n_true = 0, amplitude = 0) {
  # Generate the variables from a multivariate normal distribution
  mu <- rep(0, ncols)
  rho <- 0.25
  sigma <- toeplitz(rho^(0:(ncols - 1))) #  Symmetric Toeplitz Matrix
  x <- matrix(rnorm(nrows * ncols), nrows) %*% chol(sigma) # multiply matrices Choleski Decomposition. Description. Compute the Choleski factorization of a real symmetric positive-definite square matrix)

  # Generate the response from a linear model
  nonzero <- sample(ncols, n_true) # select the id of 'true' variables
  beta <- amplitude * (1:ncols %in% nonzero) / sqrt(nrows) # vector of effect sizes to pick out true varaiables
  beta_value <- amplitude / sqrt(nrows)
  outcome.sample <- function(x) x %*% beta + rnorm(nrows) # calculate outcome from true vars and error
  outcome <- outcome.sample(x)

  x <- as.data.frame(x)

  ## Rename true variables as "true_" and junk variables as "junk_"
  x <- x %>%
    rename_with(~ paste0("true_", .x), .cols = nonzero) %>%
    rename_with(~ paste0("junk_", .x), .cols = !nonzero)

  return(as.data.frame(cbind(outcome, x)))
}
