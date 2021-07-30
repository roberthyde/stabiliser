#' model_selector
#'
#' Main function to call both permutation and bootstrapping functions; to be looped over multiple models selected by the user.
#'
utils::globalVariables(c('data', 'outcome', 'boot_reps', 'permutations', 'perm_boot_reps', 'model_name'))

perm_stab <- function(data, outcome, boot_reps, permutations, perm_boot_reps, model_name) {
  selected_model <- model_selector(model_name)

  message("Permuting ", model_name, "...")
  perm_thresh <- permute(data = data, outcome = outcome, permutations = permutations, perm_boot_reps = perm_boot_reps, selected_model = selected_model)
  message("Done")
  message("Stabilising ", model_name, "...")
  coefs <- boot_model(data = data, outcome = outcome, boot_reps = boot_reps, selected_model = selected_model)
  stability <- boot_summarise(booted_obj = coefs, data = data, boot_reps = boot_reps) %>%
    mutate(stable = case_when(stability >= perm_thresh ~ "*"))
  message("Done")

  list(
    "stability" = stability,
    "bootstrap_coefficients" = coefs %>%
      group_by(bootstrap) %>%
      nest(),
    "perm_thresh" = perm_thresh
  )
}
