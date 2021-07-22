#' boot_model
#'
#' Function to calculate stability of variables' association with an outcome for a given model over a number of bootstrap repeats
#'
#' @param data a dataframe containing an outcome variable to be permuted
#' @param outcome the outcome as a string (i.e. "y")
#' @param boot_reps the number of bootstrap samples
#' @param model the model to be used (i.e. model_mbic)
#'
#' @import rsample
#' @import dplyr
#' @import purrr
#' @importFrom utils globalVariables
#' @importFrom tidyr replace_na
#'
utils::globalVariables(c(".", "variable", "stability"))

boot_model <- function(data, outcome, boot_reps) {
  rsample::bootstraps(data, boot_reps) %>%
    map_df(.x = .$splits, .f = ~ model_mbic(., outcome = outcome)) %>%
    group_by(variable) %>%
    summarise(mean_coefficient = mean(estimate, na.rm=TRUE),
              ci_lower = quantile(estimate, 0.025, na.rm = TRUE),
              ci_upper = quantile(estimate, 0.975, na.rm = TRUE),
              prop_one_side = case_when(mean_coefficient > 0 ~ length(estimate[estimate>0]),
                                  mean_coefficient <0 ~ length(estimate[estimate<0])),
              bootstrap_p = 1-(prop_one_side/length(estimate)),
              stability = (n() / boot_reps) * 100) %>%
    select(-prop_one_side) %>%
    right_join(tibble(variable = colnames(data))) %>%
    replace_na(list(stability = 0)) %>%
    arrange(desc(stability))
}
