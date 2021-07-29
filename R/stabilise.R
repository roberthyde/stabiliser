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
#' @export
#'

stabilise <- function(data, outcome, boot_reps, permutations, perm_boot_reps, models) {

  model_selector <- function(selected_model){
    case_when(selected_model == "lasso" ~ model_lasso,
              selected_model == "mbic" ~ model_mbic)
  }

  perm_stab <- function(data, outcome, boot_reps, permutations, perm_boot_reps, selected_model){

  selected_model <- model_selector(selected_model)
  print("Permuting...")
  perm_thresh <- permute(data = data, outcome = outcome, permutations = permutations, perm_boot_reps = perm_boot_reps, selected_model = selected_model)
  print("Permuting...done")
  print("Stabilising...")
  stability <- boot_model(data = data, outcome = outcome, boot_reps = boot_reps, selected_model = selected_model) %>%
    mutate(significant = case_when(stability >= perm_thresh ~ "*"))
  print("Stabilising...done")

  list(
    "stability" = stability,
    "perm_thresh" = perm_thresh
  )
  }

  model_names <- map(models, ~enexpr(.x))

  output <- models %>%
    map(., ~perm_stab(data = data,
                                      outcome = outcome,
                                      boot_reps=boot_reps,
                                      permutations=permutations,
                                      perm_boot_reps=perm_boot_reps,
                                      selected_model = .))

  names(output) <- model_names

  output
}
