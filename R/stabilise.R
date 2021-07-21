#' stabilise
#'
#' Function to calculate stability of variables' association with an outcome for a given model over a number of bootstrap repeats
#'
#' @data a dataframe containing an outcome variable to be permuted
#' @outcome the outcome as a string (i.e. "y")
#' @boot_reps the number of bootstrap samples
#' @model the model to be used (i.e. model_mbic)
#' @permutations the number of times to be permuted per repeat
#' @perm_boot_reps the number of times to repeat each set of permutations
#' @model the model to be used (i.e. model_mbic)
#' @... tuning parameters to be passed to model (i.e. minpv for model_mbic)
#'
#' @import rsample
#' @import dplyr
#' @import purrr
#'
#' @export
#'

stabilise <- function(data, outcome, boot_reps, permutations, perm_boot_reps, model, ...){
  perm_thresh <- permute(data=data, outcome=outcome, permutations=permutations, perm_boot_reps=perm_boot_reps, model=model)
  stability <- boot_model(data=data, outcome=outcome, boot_reps=boot_reps, model=model, ...)

  list("stability" = stability,
       "perm_thresh" = perm_thresh)
}
