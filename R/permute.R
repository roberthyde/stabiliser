#' Permute
#'
#' Permutes a specified outcome a given number of times, and repeats this several times.
#'
#' @data a dataframe containing an outcome variable to be permuted
#' @permute an outcome variable to be permuted
#' @times the number of times to repeat the permutation
#' @repeats the number of repeats of this process
#' @import dplyr
#' @importFrom purrr rerun
#' @importFrom rsample permutations

permute <-
function(data, permute, times, repeats){
  purrr::rerun(repeats, rsample::permutations(data = data, permute = permute, times = times)) %>%
    bind_rows(.id="repeat")
}

