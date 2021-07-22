#' Permute
#'
#' Calculates permutation threshold for null model, where a specified model is run over multiple bootstrap resamples of multiple permuted version of the dataset.
#'
#' @param data a dataframe containing an outcome variable to be permuted
#' @param outcome the outcome to be permuted as a string (i.e. "y")
#' @param permutations the number of times to be permuted per repeat
#' @param perm_boot_reps the number of times to repeat each set of permutations
#' @param model the model to be used (i.e. model_mbic)
#'
#' @import dplyr
#' @import purrr
#' @importFrom tidyr unnest
#' @importFrom rsample permutations
#'

permute <- function(data, outcome, permutations, perm_boot_reps, model) {
  rsample::permutations(data = data, permute = outcome, times = permutations) %>%
    mutate(
      stab_df = map(.x = .$splits, .f = ~ as.data.frame(.) %>% boot_model(., outcome = outcome, boot_reps = perm_boot_reps, model = model)),
      perm_thresh = map(stab_df, ~ as_vector(.x$stability) %>%
        ecdf() %>%
        quantile(., probs = 1))
    ) %>%
    unnest(perm_thresh) %>%
    summarise(mean_thresh = mean(perm_thresh)) %>%
    pull(mean_thresh)
}
