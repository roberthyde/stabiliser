#' Boot
#'
#' Performs bootstrap resmampling on a dataframe.
#'
#' @data a dataframe containing an outcome variable to be permuted
#' @repeats the number of bootstrap repeats
#'
#' @importFrom rsample bootstraps
#' @export

boot <-
function(data, repeats){
  rsample::bootstraps(data, repeats)
}
