#' stab_plot
#'
#' Plot from stability object
#'
#' @param stabiliser_outcome outcome from stabiliser() function
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
    ggplot(aes(x = stability, y = bootstrap_p)) +
    geom_jitter(height = 0.05, width = 1) +
    geom_vline(xintercept = model$perm_thresh) +
    labs(
      x = "Stability (%)",
      y = "Bootstrap-p"
    ) +
    scale_y_reverse()
}
