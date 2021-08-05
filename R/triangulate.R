#' triangulate
#'
#' @name triangulate
#'
#' @description Triangulate multiple models using a stability object
#'
#' @param object an object generated through the stability() function
#'
#' @import dplyr
#' @importFrom stats ecdf
#'
#' @export
#'

utils::globalVariables(c("object", "model", "perm_stabs", "permutation"))

triangulate <- function(object) {
  #Mean stability across all models for each permutation
  perm_thresh <- map_df(object, ~ .x$perm_coefs, .id = "model") %>%
    group_by(permutation, variable) %>%
    summarise(stability = mean(stability, na.rm=TRUE)) %>%
    perm_summarise()

  stability <- map_df(object, ~ .x$stability, .id = "model") %>%
    group_by(variable) %>%
    summarise(
      stability = mean(stability, na.rm=TRUE),
      bootstrap_p = mean(bootstrap_p, na.rm=TRUE)
    ) %>%
    arrange(desc(stability)) %>%
    mutate(stable = case_when(stability > perm_thresh ~ "*"))

  list(
    "combi" = list(
    "stability" = stability,
    "perm_thresh" = perm_thresh
    )
  )
}
