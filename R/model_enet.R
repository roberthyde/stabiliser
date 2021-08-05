#' model_enet
#'
#' @name model_enet
#'
#' @description Function to model elastic net selection process on a given dataframe
#'
#' @param data a dataframe containing an outcome variable to be permuted (usually coming from nested bootstrap data)
#' @param outcome the outcome as a string (i.e. "y")
#'
#' @import glmnet
#' @import dplyr
#' @import broom
#' @importFrom caret train
#' @importFrom caret trainControl
#' @importFrom tibble rownames_to_column
#' @importFrom stats coef
#' @importFrom utils globalVariables
#' @importFrom stringr str_remove_all
#'
#'
utils::globalVariables(c(".", "variable", "estimate", "value", "name", "coefficient"))

model_enet <- function(data, outcome) {
  #ctrl <- caret::trainControl(
  #  method = "repeatedcv",
  #  number = 5,
  #  repeats = 5
  #)#

  #data <- data %>%
  #  as.data.frame()

  #fit_lasso <- data %>%
  #  caret::train(y ~ .,
  #    data = .,
  #    trControl = ctrl,
  #    method = "glmnet",
  #    tuneGrid = expand.grid(alpha = 1, lambda = 0)
  #  ) # TODO Currently lambda fixed at zero

  #coef(fit_lasso$finalModel, fit_lasso$bestTune$lambda) %>%
  #  broom::tidy() %>%
  #  rename(
  #    variable = row,
  #    estimate = value
  #  ) %>%
  #  filter(variable != "(Intercept)") %>%
  #  select(variable, estimate)

  # Or use glmnet directly
  y_temp <- data %>%
    select(all_of(outcome)) %>%
    as.matrix()

  x_temp <- data %>%
    select(-all_of(outcome)) %>%
    as.matrix()

  #fit_glmnet <- glmnet(x=x_temp, y=y_temp)

  # fit_glmnet %>%
  #  broom::tidy() %>%
  #  arrange(desc(step))
  #  filter(dev.ratio == max(dev.ratio)) %>%
  #  rename(variable = term) %>%
  #  filter(variable != "(Intercept)") %>%
  #  select(variable, estimate)

  #CV fit
  fit_glmnet <- cv.glmnet(x=x_temp, y=y_temp)

  coefs <- coef(fit_glmnet, s = "lambda.min")

  data.frame(name = coefs@Dimnames[[1]][coefs@i + 1], coefficient = coefs@x) %>%
    rename(variable = name,
           estimate = coefficient) %>%
    filter(variable != "(Intercept)") %>%
    select(variable, estimate)
}
