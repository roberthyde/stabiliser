#' Permute
#'
#' Permutes a specified outcome a given number of times, and repeats this several times.
#'
#' @data a dataframe containing an outcome variable to be permuted
#' @outcome the outcome to be permuted as a string (i.e. "y")
#' @permutations the number of times to be permuted per repeat
#' @repeats the number of times to repeat each set of permutations
#' @model the model to be used (i.e. model_mbic)
#'
#' @import dplyr
#' @importFrom purrr rerun
#' @importFrom rsample permutations
#'

permute <- function(data, outcome, permutations, boot_reps, model, ...){
  rsample::permutations(data = data, permute = outcome, times = permutations) %>%
    mutate(stabs = map(.x = .$splits, .f = ~as.data.frame(.) %>% boot_model(., outcome=outcome, boot_reps = boot_reps, model=model, minpv=minpv))) %>%
    select(id, stabs)
}

test <- permute(data = sim_dat, outcome="y", permutations=5, boot_reps=2, model=model_mbic, minpv=0.05)
