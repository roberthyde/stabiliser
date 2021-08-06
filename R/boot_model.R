#' boot_model
#'
#' @name boot_model
#'
#' @description Function to calculate stability of variables' association with an outcome for a given model over a number of bootstrap repeats
#'
#' @param data a dataframe containing an outcome variable to be permuted
#' @param outcome the outcome as a string (i.e. "y")
#' @param boot_reps the number of bootstrap samples
#' @param model the model to be used (i.e. model_mbic)
#'
#' @import rsample
#' @import dplyr
#' @importFrom purrr map_df
#' @importFrom utils globalVariables
#' @importFrom tidyr replace_na
#' @importFrom stats quantile
#'

utils::globalVariables(c(".", "variable", "stability", "estimate", "quantile", "prop_one_side", "bootstrap_p"))

boot_sample <- function(data, boot_reps) {
  rsample::bootstraps(data, boot_reps)
}

boot_model <- function(data, outcome, boot_reps, selected_model) {
  data %>%
    map_df(.x = .$splits, .f = ~ as.data.frame(.) %>% selected_model(., outcome = outcome), .id = "bootstrap")
}

boot_summarise <- function(booted_obj, data, boot_reps) {
  booted_obj %>%
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
    right_join(tibble(variable = colnames(data)), by = "variable") %>%
    replace_na(list(
      stability = 0
    )) %>%
    arrange(desc(stability))
}
