#' model_mcp
#'
#' @name model_mcp
#'
#' @description Function to model mcp selection process on a given dataframe
#'
#' @param data a dataframe containing an outcome variable to be permuted (usually coming from nested bootstrap data)
#' @param outcome the outcome as a string (i.e. "y")
#' @param type model type, either "linear" or "logistic"
#' @keywords internal
#' @import glmnet
#' @import dplyr
#' @import ncvreg
#' @import broom
#' @importFrom tibble rownames_to_column
#' @importFrom stats coef
#' @importFrom utils globalVariables
#' @importFrom stringr str_remove_all
#'
#'

utils::globalVariables(c(".", "variable", "estimate", "x"))

model_mcp <- function(data, outcome, type) {
  data <- data %>%
    as.data.frame()

  y_temp <- data %>%
    select(all_of(outcome)) %>%
    as.matrix()

  x_temp <- data %>%
    select(-all_of(outcome))

  fit_mcp <- cv.ncvreg(X = x_temp, y = y_temp)

  fit_mcp %>%
    coef() %>%
    as_tibble(rownames = "variable") %>%
    rename(
      estimate = value
    ) %>%
    filter(
      variable != "(Intercept)",
      estimate != 0
    )
}
