#' model_mbic
#'
#' Function to model mbic selection process on a given dataframe
#'
#' @data a dataframe containing an outcome variable to be permuted (usually coming from nested bootstrap data)
#' @outcome the outcome as a string (i.e. "y")
#' @minpv the minpv value for initial filtering
#'
#' @import bigstep
#' @import dplyr
#' @importFrom tibble rownames_to_column
#'

model_mbic <- function(data, outcome, minpv){

  data <- data %>%
    as.data.frame()

  y_temp <-data %>%
    select(all_of(outcome)) %>%
    as.matrix()

  x_temp <- data %>%
    select(-all_of(outcome))

  bigstep_prepped <- bigstep::prepare_data(y_temp, x_temp, verbose = FALSE)

  bigstep_prepped %>%
    reduce_matrix(minpv = minpv) %>%
    fast_forward(crit=mbic) %>%
    multi_backward(crit=mbic) %>%
    summary() %>%
    coef() %>%
    as.data.frame() %>%
    rownames_to_column(., var = "variable") %>%
    filter(!grepl("(Intercept)", variable),
           !grepl("`Xm[, -1]`", variable))
}
