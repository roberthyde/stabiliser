#' model_mbic
#'
#' Function to model mbic selection process on a given dataframe
#'
#' @param data a dataframe containing an outcome variable to be permuted (usually coming from nested bootstrap data)
#' @param outcome the outcome as a string (i.e. "y")
#'
#' @import bigstep
#' @import dplyr
#' @importFrom tibble rownames_to_column
#' @importFrom stats coef

model_mbic <- function(data, outcome) {
  data <- data %>%
    as.data.frame()

  y_temp <- data %>%
    select(all_of(outcome)) %>%
    as.matrix()

  x_temp <- data %>%
    select(-all_of(outcome))

  bigstep_prepped <- bigstep::prepare_data(y_temp, x_temp, verbose = FALSE)

  bigstep_prepped %>%
    reduce_matrix(minpv = 0.05) %>%
    fast_forward(crit = mbic) %>%
    multi_backward(crit = mbic) %>%
    summary() %>%
    stats::coef() %>%
    as.data.frame() %>%
    rownames_to_column(.data, var = "variable") %>%
    filter(
      !grepl("(Intercept)", .data$variable),
      !grepl("`Xm[, -1]`", .data$variable)
    )
}

