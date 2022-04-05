#' model_lasso
#'
#' @name model_lasso
#'
#' @description Function to model lasso selection process on a given dataframe
#'
#' @param data a dataframe containing an outcome variable to be permuted (usually coming from nested bootstrap data)
#' @param outcome the outcome as a string (i.e. "y")
#' @param type model type, either "linear" or "logistic"
#' @keywords internal
#' @import dplyr
#' @import broom
#' @importFrom glmnet cv.glmnet
#' @importFrom caret train
#' @importFrom caret trainControl
#' @importFrom tibble rownames_to_column
#' @importFrom stats coef
#' @importFrom utils globalVariables
#' @importFrom stringr str_remove_all
#'
#'
utils::globalVariables(c(".", "variable", "estimate", "value", "1"))

model_lasso <- function(data, outcome, type) {
  type <- case_when(
    type == "logistic" ~ "binomial",
    type == "linear" ~ "gaussian"
  )

  y_temp <- data %>%
    select(all_of(outcome)) %>%
    as.matrix()

  x_temp <- data %>%
    select(-all_of(outcome)) %>%
    as.matrix()

  fit_lasso <- cv.glmnet(x = x_temp, y = y_temp, alpha = 1, family = type)

  coefs <- coef(fit_lasso, s = "lambda.min")

  data.frame(name = coefs@Dimnames[[1]][coefs@i + 1], coefficient = coefs@x) %>%
    rename(
      variable = name,
      estimate = coefficient
    ) %>%
    filter(variable != "(Intercept)") %>%
    select(variable, estimate)
}
