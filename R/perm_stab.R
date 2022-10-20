#' perm_stab
#'
#' @name perm_stab
#'
#' @description Main function to call both permutation and bootstrapping functions; to be looped over multiple models selected by the user.
#' @keywords internal
#' @importFrom tidyr nest
#'

utils::globalVariables(c("data", "outcome", "boot_reps", "permutations", "perm_boot_reps", "model_name"))

perm_stab <- function(data, boot_data, perm_data, outcome, boot_reps, permutations, perm_boot_reps, model_name, type, quantile) {
  try({
    selected_model <- model_selector(model_name)
    # TODO Progress bar? Estimate time based on one bootstrap repeat?
    message("Permuting ", model_name, "...")
    perm_coefs <- perm_model(perm_data = perm_data, data = data, outcome = outcome, perm_boot_reps = perm_boot_reps, selected_model = selected_model, type = type)
    perm_thresh <- perm_summarise(permed_object = perm_coefs, quantile = quantile)
    message("Done")
    message("Stabilising ", model_name, "...")
    coefs <- boot_model(data = boot_data, outcome = outcome, selected_model = selected_model, type = type)
    stability <- boot_summarise(booted_obj = coefs, data = data, outcome = outcome, boot_reps = boot_reps) %>%
      mutate(stable = case_when(stability >= perm_thresh ~ "*"))
    message("Done")

    list(
      "stability" = stability %>%
        filter(variable != "(Intercept)"),
      "intercept" = stability %>%
        filter(variable == "(Intercept)"),
      "boot_coefs" = coefs %>%
        group_by(bootstrap) %>%
        nest() %>%
        rename(variables = data),
      "perm_thresh" = perm_thresh,
      "perm_coefs" = perm_coefs,
      "variable_names" = data %>%
        select(-outcome) %>%
        colnames()
    )
  })
}
