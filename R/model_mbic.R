#' model_mbic
#'
#' @import bigstep
#' @import dplyr
#' @importFrom tibble rownames_to_column
#' @importFrom stats coef
#' @importFrom utils globalVariables
#' @importFrom stringr str_remove_all
#'
# TODO Remove descriptions etc from non-exported functions

utils::globalVariables(c(".", "variable", "Estimate"))

model_mbic <- function(data, outcome, type) {
  data <- data %>%
    as.data.frame()
  # TODO all_of might not work with doparralel, perhaps change to !!outcome ? see tidymodels parralel explanation.
  y_temp <- data %>%
    select(all_of(outcome)) %>%
    as.matrix()

  x_temp <- data %>%
    select(-all_of(outcome))

  bigstep_prepped <- bigstep::prepare_data(y_temp, x_temp, verbose = FALSE, type = type)

  bigstep_prepped %>%
    reduce_matrix(minpv = 0.01) %>%
    fast_forward(crit = mbic) %>%
    multi_backward(crit = mbic) %>%
    summary() %>%
    stats::coef() %>%
    as.data.frame() %>%
    rownames_to_column(., var = "variable") %>%
    mutate(variable = str_remove_all(variable, "`")) %>%
    filter(
      !grepl("(Intercept)", variable),
      !grepl("Xm[, -1]", variable)
    ) %>%
    rename(estimate = Estimate) %>%
    select(variable, estimate)
}
