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
#' @importFrom stats quantile
#'

utils::globalVariables(c(".", "variable", "stability", "estimate", "quantile", "prop_one_side", "bootstrap_p"))

boot_model <- function(data, outcome, boot_reps, selected_model) {
  bootstrap_list <- map(1:boot_reps, ~data %>% sample_frac(., 1, replace=TRUE))

  #rsample::bootstraps(data, boot_reps) %>%
  bootstrap_list %>%
    map_df(.x = ., .f = ~ as.data.frame(.) %>% selected_model(., outcome = outcome)) %>%
    group_by(variable) %>%
    summarise(mean_coefficient = mean(estimate, na.rm=TRUE),
              ci_lower = quantile(estimate, 0.025, na.rm = TRUE),
              ci_upper = quantile(estimate, 0.975, na.rm = TRUE),
              prop_one_side = case_when(mean_coefficient > 0 ~ length(estimate[estimate>0]),
                                  mean_coefficient <0 ~ length(estimate[estimate<0])),
              bootstrap_p = 1-(prop_one_side/length(estimate)),
              stability = (n() / boot_reps) * 100) %>%
    select(-prop_one_side) %>%
    right_join(tibble(variable = colnames(data)), by="variable") %>%
    replace_na(list(stability = 0,
                    bootstrap_p = 1)) %>%
    arrange(desc(stability))
}
