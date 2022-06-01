#' stabiliser_prep
#'
#' @name stabiliser_prep
#'
#' @description Prepares dataset using recipes framework
#'
#' @param normalise Normalise numeric variables (TRUE/FALSE)
#' @param dummy Create dummy variables for factors/characters (TRUE/FALSE)
#' @param impute Impute missing data (TRUE/FALSE)
#'
#' @keywords internal
#' @importFrom recipes recipe
#' @importFrom recipes update_role
#' @importFrom recipes step_normalize
#' @importFrom recipes step_dummy
#' @importFrom recipes step_impute_knn
#' @importFrom recipes all_numeric_predictors
#' @importFrom recipes all_nominal_predictors
#' @importFrom recipes all_predictors
#' @importFrom recipes prep
#' @importFrom recipes juice
#' @import dplyr
#'
#'

utils::globalVariables(c(
  "formula", "all_numeric_predictors", "all_nominal_predictors",
  "all_predictors"
))

prep_data <- function(outcome, data, normalise, dummy, impute) {
  message("Prepping...")

  f <- formula(paste0(enquo(outcome), " ~ ."))

  recipe <- data %>%
    recipe(f, data = .) %>%
    update_role(outcome,
      new_role = "outcome"
    )

  if (normalise == TRUE) {
    recipe <- recipe %>%
      step_normalize(all_numeric_predictors())
  }

  if (dummy == TRUE) {
    recipe <- recipe %>%
      step_dummy(all_nominal_predictors())
  }

  if (impute == TRUE) {
    recipe <- recipe %>%
      step_impute_knn(all_predictors())
  }

  print(recipe)

  data_prepped <- recipe %>%
    prep() %>%
    juice()

  return(data_prepped)
}
