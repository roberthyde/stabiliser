#' stabilise
#'
#' Function to calculate stability of variables' association with an outcome for a given model over a number of bootstrap repeats
#'
#' @param data a dataframe containing an outcome variable to be permuted
#' @param outcome the outcome as a string (i.e. "y")
#' @param boot_reps the number of bootstrap samples
#' @param permutations the number of times to be permuted per repeat
#' @param perm_boot_reps the number of times to repeat each set of permutations
#'
#' @import rsample
#' @import dplyr
#' @import purrr
#'
#' @export
#'

stabilise <- function(data, outcome, boot_reps, permutations, perm_boot_reps) {
  print("Permuting...")
  perm_thresh <- permute(data = data, outcome = outcome, permutations = permutations, perm_boot_reps = perm_boot_reps)
  print("Permuting...done")
  print("Stabilising...")
  stability <- boot_model(data = data, outcome = outcome, boot_reps = boot_reps) %>%
    mutate(significant = case_when(stability >= perm_thresh ~ "*"))
  print("Stabilising...done")

  list(
    "stability" = stability,
    "perm_thresh" = perm_thresh
  )
}
