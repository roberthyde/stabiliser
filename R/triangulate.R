#' triangulate
#'
#' @name triangulate
#'
#' @description Triangulate multiple models using a stability object
#'
#' @param object An object generated through the stabilise() function.
#' @param quantile The quantile of null stabilities to use as a threshold.
#'
#' @return A combined list of model results including a dataframe of stability results for variables and a numeric permutation threshold.
#'
#' @import dplyr
#' @importFrom stats ecdf
#'
#' @export
#'

utils::globalVariables(c("object", "model", "perm_stabs", "permutation"))

triangulate <- function(object, quantile=1) {
  # Mean stability across all models for each permutation
  perm_thresh <- map_df(object, ~ .x$perm_coefs, .id = "model") %>%
    group_by(permutation, variable) %>%
    summarise(stability = mean(stability, na.rm = TRUE)) %>%
    perm_summarise(permed_object = ., quantile=quantile)

  stability <- map_df(object, ~ .x$stability, .id = "model") %>%
    group_by(variable) %>%
    summarise(
      stability = mean(stability, na.rm = TRUE),
      bootstrap_p = mean(bootstrap_p, na.rm = TRUE)
    ) %>%
    arrange(desc(stability)) %>%
    mutate(stable = case_when(stability >= perm_thresh ~ "*"))

  list(
    "combi" = list(
      "stability" = stability,
      "perm_thresh" = perm_thresh
    )
  )
}
