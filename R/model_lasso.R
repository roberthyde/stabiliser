#' model_lasso
#'
#' Function to model mbic selection process on a given dataframe
#'
#' @param data a dataframe containing an outcome variable to be permuted (usually coming from nested bootstrap data)
#' @param outcome the outcome as a string (i.e. "y")
#'
#' @import glmnet
#' @import dplyr
#' @import broom
#' @importFrom tibble rownames_to_column
#' @importFrom stats coef
#' @importFrom utils globalVariables
#' @importFrom stringr str_remove_all
#'
#' @export
#'
utils::globalVariables(c(".", "variable", "estimate"))

model_lasso <- function(data, outcome) {
  data <- data %>%
    as.data.frame() %>%
    sample_frac(., size=1, replace=TRUE)

  y_temp <- data %>%
    select(all_of(outcome)) %>%
    as.matrix()

  x_temp <- data %>%
    select(-all_of(outcome))

  fit_lasso <- cv.glmnet(x=x_temp, y=y_temp, alpha = 1, type.measure = "mae", nfolds = 5)

  coef(fit_lasso, s = "lambda.min") %>%
    broom::tidy() %>%
    rename(variable = row,
           estimate = value) %>%
    filter(variable != "(Intercept)") %>%
    select(variable, estimate)
  }

