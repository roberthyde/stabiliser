#' permute
#'
#' @name permute
#'
#' @description Calculates permutation threshold for null model, where a specified model is run over multiple bootstrap resamples of multiple permuted version of the dataset.
#'
#' @param data a dataframe containing an outcome variable to be permuted
#' @param outcome the outcome to be permuted as a string (i.e. "y")
#' @param permutations the number of times to be permuted per repeat
#' @param perm_boot_reps the number of times to repeat each set of permutations
#'
#' @import dplyr
#' @importFrom purrr map
#' @importFrom tidyr unnest
#' @importFrom rsample permutations
#' @importFrom utils globalVariables
#'
#'
utils::globalVariables(c("stab_df", "perm_thresh", "mean_thresh", "perm_coefs", "perm_stabs", "splits", "permutation"))

perm_sample <- function(data, outcome, permutations, perm_boot_reps) {
  rsample::permutations(data = data, permute = outcome, times = permutations) %>%
    map_df(.x = .$splits, .f = ~ as.data.frame(.) %>% boot_sample(., perm_boot_reps), .id = "permutation")
}

perm_model <- function(perm_data, data, outcome, permutations, perm_boot_reps, selected_model) {
  perm_data %>%
    mutate(perm_coefs = map(.x = .$splits, .f = ~ as.data.frame(.) %>%
      selected_model(., outcome = outcome), .id = "bootstrap")) %>%
    select(-splits) %>%
    unnest(perm_coefs) %>%
    select(-id) %>%
    group_by(permutation) %>%
    nest() %>%
    rename(perm_data = data) %>%
    map_df(.x = .$perm_data, .f = ~ as.data.frame(.) %>%
      boot_summarise(booted_obj = ., data = data, boot_reps = perm_boot_reps), .id = "permutation")
}

perm_summarise <- function(permed_object) {
  permed_object %>%
    group_by(permutation) %>%
    summarise(perm_thresh = as.vector(stability) %>%
      ecdf() %>%
      quantile(., probs = 1)) %>%
    summarise(mean_thresh = mean(perm_thresh, na.rm = TRUE)) %>%
    pull(mean_thresh)
}
