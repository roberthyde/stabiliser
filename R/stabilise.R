#' stabilise
#'
#' @name stabilise
#'
#' @description Function to calculate stability of variables' association with an outcome for a given model over a number of bootstrap repeats
#'
#' @param data A dataframe containing an outcome variable to be permuted.
#' @param outcome The outcome as a string (i.e. "y").
#' @param boot_reps The number of bootstrap samples. Default is "auto" which selects number based on dataframe size.
#' @param permutations The number of times to be permuted per repeat. Default is "auto" which selects number based on dataframe size.
#' @param perm_boot_reps The number of times to repeat each set of permutations. Default is 5.
#' @param models The models to select for stabilising. Default is elastic net (models = c("enet")), other available models include "lasso", "mbic", "mcp".
#' @param type The type of model, either "linear" or "logistic"
#' @param quantile The quantile of null stabilities to use as a threshold.
#' @param normalise Normalise numeric variables (TRUE/FALSE)
#' @param dummy Create dummy variables for factors/characters (TRUE/FALSE)
#' @param impute Impute missing data (TRUE/FALSE)
#'
#'
#' @return A list for each model selected. Each list contains a dataframe of variable stabilities, a numeric permutation threshold, and a dataframe of coefficients for both bootstrap and permutation.
#'
#' @import rsample
#' @import dplyr
#' @importFrom purrr map
#'
#' @export
#'
utils::globalVariables(c("models"))

stabilise <- function(data, outcome, boot_reps = "auto", permutations = "auto", perm_boot_reps = 20,
                      models = c("enet"), type = "linear", quantile = 1, normalise = TRUE, dummy = TRUE, impute = TRUE) {
  data <- prep_data(data = data, outcome = outcome, normalise = normalise, dummy = dummy, impute = impute)

  boot_reps <- rep_selector_boot(data = data, boot_reps = boot_reps)
  permutations <- rep_selector_perm(data = data, permutations = permutations)

  message("Stabilising across ", boot_reps, " bootstrap resamples. Permuting ", permutations, " times, with ", perm_boot_reps, " bootstrap samples for each permutation.")

  boot_data <- boot_sample(data = data, boot_reps = boot_reps)
  perm_data <- perm_sample(data = data, outcome = outcome, permutations = permutations, perm_boot_reps = perm_boot_reps)

  output <- models %>%
    map(., ~ perm_stab(
      data = data,
      boot_data = boot_data,
      perm_data = perm_data,
      outcome = outcome,
      boot_reps = boot_reps,
      permutations = permutations,
      perm_boot_reps = perm_boot_reps,
      model_name = .,
      type = type,
      quantile = quantile
    ))

  names(output) <- models

  output
}
