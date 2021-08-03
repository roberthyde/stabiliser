#' perm_stab
#'
#' @name perm_stab
#'
#' @description Main function to call both permutation and bootstrapping functions; to be looped over multiple models selected by the user.
#'
#' @importFrom tidyr nest
#'

utils::globalVariables(c('data', 'outcome', 'boot_reps', 'permutations', 'perm_boot_reps', 'model_name'))

perm_stab <- function(data, boot_data, perm_data, outcome, boot_reps, permutations, perm_boot_reps, model_name) {
  selected_model <- model_selector(model_name)

  message("Permuting ", model_name, "...")
  perm_coefs <- perm_model(perm_data = perm_data, data = data, outcome = outcome, permutations = permutations, perm_boot_reps = perm_boot_reps, selected_model = selected_model)
  perm_thresh <- perm_summarise(permed_object = perm_coefs)
  message("Done")
  message("Stabilising ", model_name, "...")
  coefs <- boot_model(data = boot_data, outcome = outcome, boot_reps = boot_reps, selected_model = selected_model)
  stability <- boot_summarise(booted_obj = coefs, data = data, boot_reps = boot_reps) %>%
    mutate(stable = case_when(stability >= perm_thresh ~ "*"))
  message("Done")

  list(
    "stability" = stability,
    "boot_coefs" = coefs %>%
      group_by(bootstrap) %>%
      nest() %>%
      rename(variables = data),
    "perm_thresh" = perm_thresh,
    "perm_coefs" = perm_coefs
    )
}