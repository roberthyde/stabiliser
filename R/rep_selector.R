#' rep_selector_boot
#'
#' @name rep_selector_boot
#'
#' @description wrapper function to determine the number of bootstrap repeats
#'
#' @param data the dataset to analyse.
#' @param boot_reps the number of bootstrap samples
#' @keywords internal
#' @import dplyr
#'

rep_selector_boot <- function(data, boot_reps) {
  if (boot_reps == "auto") {
    if (nrow(data) < 200) {
      200
    } else {
      100
    }
  } else {
    boot_reps
  }
}

#' rep_selector_boot
#'
#' @name rep_selector_perm
#'
#' @description wrapper function to determine the number of permutations
#'
#' @param data the dataset to analyse.
#' @param permutations the number of times to be permuted per repeat
#' @keywords internal

rep_selector_perm <- function(data, permutations) {
  if (permutations == "auto") {
    if (nrow(data) < 200) {
      10
    } else {
      5
    }
  } else {
    permutations
  }
}
