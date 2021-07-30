#' model_mcp
#'
#' Function to model mcp selection process on a given dataframe
#'
#' @param data a dataframe containing an outcome variable to be permuted (usually coming from nested bootstrap data)
#' @param outcome the outcome as a string (i.e. "y")
#'
#' @import glmnet
#' @import dplyr
#' @import ncvreg
#' @importFrom broom tidy
#' @importFrom tibble rownames_to_column
#' @importFrom stats coef
#' @importFrom utils globalVariables
#' @importFrom stringr str_remove_all
#'
#'

utils::globalVariables(c(".", "variable", "estimate", "x"))

model_mcp <- function(data, outcome) {
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
    broom::tidy() %>%
    rename(
      variable = names,
      estimate = x
    ) %>%
    filter(
      variable != "(Intercept)",
      estimate != 0
    )
}
