#' Permute
#'
#' Calculates permutation threshold for null model, where a specified model is run over multiple bootstrap resamples of multiple permuted version of the dataset.
#'
#' @data a dataframe containing an outcome variable to be permuted
#' @outcome the outcome to be permuted as a string (i.e. "y")
#' @permutations the number of times to be permuted per repeat
#' @repeats the number of times to repeat each set of permutations
#' @model the model to be used (i.e. model_mbic)
#'
#' @import dplyr
#' @import purrr
#' @importFrom rsample permutations
#'

permute <- function(data, outcome, permutations, boot_reps, model, ...){
  rsample::permutations(data = data, permute = outcome, times = permutations) %>%
    mutate(stab_df = map(.x = .$splits, .f = ~as.data.frame(.) %>% boot_model(., outcome=outcome, boot_reps = boot_reps, model=model, minpv=minpv)),
           perm_thresh = map(stab_df, ~as_vector(.x$stability) %>% ecdf() %>% quantile(., probs=1))) %>%
    unnest(perm_thresh) %>%
    summarise(mean_perm_thresh = mean(perm_thresh))
}
