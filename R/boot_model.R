#' boot_model
#'
#' Function to calculate stability of variables' association with an outcome for a given model over a number of bootstrap repeats
#'
#' @param data a dataframe containing an outcome variable to be permuted
#' @param outcome the outcome as a string (i.e. "y")
#' @param boot_reps the number of bootstrap samples
#' @param model the model to be used (i.e. model_mbic)
#' @param ... further model specific paramaters (i.e. minpv=0.01 for model_mbic)
#'
#' @import rsample
#' @import dplyr
#' @import purrr
#' @importFrom tidyr replace_na
#'

boot_model <- function(data, outcome, boot_reps, model) {
  rsample::bootstraps(data, boot_reps) %>%
    map_df(.x = .$splits, .f = ~ model(., outcome = outcome)) %>%
    group_by(variable) %>%
    summarise(stability = (n() / boot_reps) * 100) %>%
    right_join(tibble(variable = colnames(data))) %>%
    replace_na(list(stability = 0)) %>%
    arrange(desc(stability))
}
