#' stabilise
#'
#' Function to calculate stability of variables' association with an outcome for a given model over a number of bootstrap repeats
#'
#' @param data a dataframe containing an outcome variable to be permuted
#' @param outcome the outcome as a string (i.e. "y")
#' @param boot_reps the number of bootstrap samples
#' @param model the model to be used (i.e. model_mbic)
#' @param permutations the number of times to be permuted per repeat
#' @param perm_boot_reps the number of times to repeat each set of permutations
#' @param model the model to be used (i.e. model_mbic)
#'
#' @import rsample
#' @import dplyr
#' @import purrr
#'
#' @export
#'

stabilise <- function(data, outcome, boot_reps, permutations, perm_boot_reps) {
  perm_thresh <- permute(data = data, outcome = outcome, permutations = permutations, perm_boot_reps = perm_boot_reps, model = model_mbic)
  stability <- boot_model(data = data, outcome = outcome, boot_reps = boot_reps, model = model_mbic)

  list(
    "stability" = stability,
    "perm_thresh" = perm_thresh
  )
}
