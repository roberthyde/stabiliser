#' permute
#'
#' Calculates permutation threshold for null model, where a specified model is run over multiple bootstrap resamples of multiple permuted version of the dataset.
#'
#' @param data a dataframe containing an outcome variable to be permuted
#' @param outcome the outcome to be permuted as a string (i.e. "y")
#' @param permutations the number of times to be permuted per repeat
#' @param perm_boot_reps the number of times to repeat each set of permutations
#'
#' @import dplyr
#' @import purrr
#' @importFrom tidyr unnest
#' @importFrom rsample permutations
#' @importFrom utils globalVariables
#'
#'
utils::globalVariables(c("stab_df", "perm_thresh", "mean_thresh"))

permute <- function(data, outcome, permutations, perm_boot_reps, selected_model) {
  perm_list <- map(1:permutations, ~data %>% mutate(y = sample(y)))

  #rsample::permutations(data = data, permute = outcome, times = permutations)
  stab_df <- perm_list %>%
    map(.x = ., .f = ~ boot_model(., outcome = outcome, boot_reps = perm_boot_reps, selected_model = selected_model))

  perm_thresh <- map(stab_df, ~ as_vector(.x$stability) %>%
                          ecdf() %>%
                          quantile(., probs = 1)) %>%
    bind_rows() %>%
    summarise(mean_thresh = mean(perm_thresh)) %>%
    pull(mean_thresh)
}
