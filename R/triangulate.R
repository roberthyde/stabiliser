#' triangulate
#'
#' @name triangulate
#'
#' @description Triangulate multiple models using a stability object
#'
#' @param object An object generated through the stabilise() function.
#' @param quantile The quantile of null stabilities to use as a threshold.
#'
#' @return A combined list of model results including a dataframe of stability results for variables and a numeric permutation threshold.
#'
#' @import dplyr
#' @importFrom stats ecdf
#'
#' @export
#'

utils::globalVariables(c("object", "model", "perm_stabs", "permutation"))

triangulate <- function(object, quantile = 1) {
  # Mean stability across all models for each permutation
  perm_thresh <- map_df(object, ~ .x$perm_coefs, .id = "model") %>%
    group_by(permutation, variable) %>%
    summarise(stability = mean(stability, na.rm = TRUE)) %>%
    perm_summarise(permed_object = ., quantile = quantile)

  variables <- tibble(variable = object[[1]]$variable_names)
  number_models <- length(object)
  boot_reps <- nrow(object[[1]]$boot_coefs) * number_models

  stability <- map_df(object, ~ .x$boot_coefs, .id = "model") %>%
    unnest(cols=variables) %>%
    group_by(variable) %>%
    summarise(
      mean_coefficient = mean(estimate, na.rm = TRUE),
      ci_lower = quantile(estimate, 0.025, na.rm = TRUE),
      ci_upper = quantile(estimate, 0.975, na.rm = TRUE),
      prop_one_side = case_when(
        mean_coefficient > 0 ~ length(estimate[estimate > 0]),
        mean_coefficient < 0 ~ length(estimate[estimate < 0])
      ),
      bootstrap_p = 1 - (prop_one_side / length(estimate)),
      stability = (n() / boot_reps) * 100
    ) %>%
    select(-prop_one_side) %>%
    right_join(variables, by = "variable") %>%
    replace_na(list(
      stability = 0
    )) %>%
    arrange(desc(stability)) %>%
    mutate(stable = case_when(stability >= perm_thresh ~ "*"))

  list(
    "combi" = list(
      "stability" = stability,
      "perm_thresh" = perm_thresh
    )
  )
}
