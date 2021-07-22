#' stab_plot
#'
#' Plot from stability object
#'
#' @param stabiliser_outcome outcome from stabiliser() function
#'
#' @import ggplot2
#' @export
#'

stab_plot <- function(stabiliser_outcome){
  stabiliser_outcome$stability %>%
    ggplot(aes(x=stability, y=bootstrap_p))+
    geom_jitter(height = 0.05, width=1)+
    geom_vline(xintercept=stabiliser_outcome$perm_thresh)+
    labs(x = "Stability (%)",
         y = "Bootstrap-p")+
    scale_y_reverse()
}
