#' stab_plot
#'
#' @name stab_plot
#'
#' @description Plot from stability object
#'
#' @param stabiliser_outcome Outcome from stabilise() or triangulate() function.
#'
#' @import ggplot2
#' @importFrom purrr map
#' @export
#'
utils::globalVariables(c("stabiliser_outcome"))

stab_plot <- function(stabiliser_object) {
  map(stabiliser_object, ~ stab_boot_plot(.))
}

stab_boot_plot <- function(model) {
  model$stability %>%
    filter(!is.na(bootstrap_p)) %>%
    ggplot(aes(x = stability, y = bootstrap_p)) +
    geom_jitter(height = 0.05, width = 1) +
    geom_vline(xintercept = model$perm_thresh) +
    labs(
      x = "Stability (%)",
      y = "Bootstrap-p"
    ) +
    scale_y_reverse()+
    theme_minimal()
}
