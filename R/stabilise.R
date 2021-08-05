#' stabilise
#'
#' @name stabilise
#'
#' @description Function to calculate stability of variables' association with an outcome for a given model over a number of bootstrap repeats
#'
#' @param data a dataframe containing an outcome variable to be permuted
#' @param outcome the outcome as a string (i.e. "y")
#' @param boot_reps the number of bootstrap samples
#' @param permutations the number of times to be permuted per repeat
#' @param perm_boot_reps the number of times to repeat each set of permutations
#'
#' @import rsample
#' @import dplyr
#' @importFrom purrr map
#'
#' @export
utils::globalVariables(c("models"))

stabilise <- function(data, outcome, boot_reps=100, permutations=5, perm_boot_reps=20, models=c("enet")) {

  boot_data = boot_sample(data=data, boot_reps=boot_reps)
  perm_data = perm_sample(data=data, outcome=outcome, permutations = permutations, perm_boot_reps = perm_boot_reps)

  output <- models %>%
    map(., ~ perm_stab(
      data = data,
      boot_data = boot_data,
      perm_data = perm_data,
      outcome = outcome,
      boot_reps = boot_reps,
      permutations = permutations,
      perm_boot_reps = perm_boot_reps,
      model_name = .
    ))

  names(output) <- models

  output
}
