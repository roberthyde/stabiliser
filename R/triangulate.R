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

utils::globalVariables(c("object", "model", "perm_stabs"))

triangulate <- function(object) {
  #Mean stability across all models for each permutation
  perm_mean_stab <- map_df(object, ~ .x$perm_coefs, .id = "model") %>%
    select(model, perm_stabs) %>%
    unnest(cols = c(perm_stabs)) %>%
    group_by(variable) %>%
    summarise(stability = mean(stability, na.rm=TRUE)) %>%
    arrange(desc(stability))

  #Calculate perm threshold across all of these
  perm_thresh <- as.vector(perm_mean_stab$stability) %>%
    ecdf() %>%
    quantile(., probs = 1) %>%
    as.numeric()

  stability <- map_df(object, ~ .x$stability, .id = "model") %>%
    group_by(variable) %>%
    summarise(stability = sum(stability, na.rm = TRUE) / length(object)) %>%
    arrange(desc(stability)) %>%
    mutate(stable = case_when(stability >= perm_thresh ~ "*"))

  list(
    "stability" = stability,
    "perm_thresh" = perm_thresh
  )
}
