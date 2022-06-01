#' simulate_selection_bias
#'
#' @name simulate_selection_bias
#'
#' @description An function to illustrate the risk of selection bias in
#' conventional modelling approaches by simulating a dataset with no information and
#' conducting conventional modelling with prefiltration.
#'
#' @param nrows A vector of the number of rows to simulate (i.e., c(100, 200)).
#' @param ncols A vector of the number of columns to simulate (i.e., c(100, 200)).
#' @param p_thresh A vector of the p-value threshold to use in univariate pre-filtration (i.e., c(0.1, 0.2)).
#'
#' @return A list including a dataframe of results, a dataframe of the median number of variables selected and a plot illustrating false positive selection.
#'
#' @import dplyr
#' @import purrr
#' @importFrom  Hmisc rcorr
#' @importFrom tidyr expand_grid
#' @importFrom stats median
#' @importFrom stats runif
#'
#' @export
#'
utils::globalVariables(c("nmbr_sel", "median"))

simulate_selection_bias <- function(ncols, nrows, p_thresh, repeats) {
  params <- expand_grid(
    ncols = ncols,
    nrows = nrows,
    p_thresh = p_thresh
  )

  results <- rerun(
    .n = repeats,
    pmap_dfr(params, selection_bias_inner)
  ) %>%
    bind_rows(., .id = "rep")

  results$p_thresh <- as.factor(results$p_thresh)

  median_nmber_selecetd <- results %>%
    dplyr::group_by(nrows, ncols, p_thresh) %>%
    dplyr::summarise(median = median(nmbr_sel), n = n())


  box_p_medians <- ggplot(results, aes(y = nmbr_sel, x = p_thresh)) +
    geom_boxplot() +
    ylim(0, 90) +
    xlab("Filter P Value") +
    ylab("Mean False Positives Selected (over 50 simulations)") +
    geom_text(
      data = median_nmber_selecetd, aes(
        y = median,
        label = round(median, 2)
      ),
      size = 3, vjust = -12, colour = "blue"
    ) +
    theme_bw() +
    facet_grid(nrows ~ ncols, labeller = label_both) +
    theme(panel.grid.major.x = element_blank())

  out <- list(
    "results" = results,
    "median_nmber_selecetd" = median_nmber_selecetd,
    "plot" = box_p_medians
  )

  return(out)
}
