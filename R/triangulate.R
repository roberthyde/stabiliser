#' triangulate
#'
#' Triangulate multiple models using a stability object
#'
#' @param object an object generated through the stability() function
#'
#' @import dplyr
#'
#' @export


triangulate <- function(object) {
  # TODO recalculate based on original matrix
  perm_thresh <- map_df(stab_output, ~ .x$perm_thresh, .id = "model") %>%
    gather(key, value) %>%
    summarise(perm_thresh = mean(value, na.rm = TRUE)) %>%
    pull(perm_thresh)

  stability <- map_df(stab_output, ~ .x$stability, .id = "model") %>%
    group_by(variable) %>%
    summarise(stability = sum(stability, na.rm = TRUE) / length(stab_output)) %>%
    arrange(desc(stability)) %>%
    mutate(stable = case_when(stability >= perm_thresh ~ "*"))

  list(
    "stability" = stability,
    "perm_thresh" = perm_thresh
  )
}
