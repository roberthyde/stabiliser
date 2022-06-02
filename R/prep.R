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
#' @importFrom recipes recipe update_role step_normalize step_dummy step_impute_knn all_numeric_predictors all_nominal_predictors all_predictors prep juice
#' @import dplyr
#'
#'

utils::globalVariables(c(
  "formula", "all_numeric_predictors", "all_nominal_predictors",
  "all_predictors"
))

prep_data <- function(outcome, data, normalise, dummy, impute) {
  message("Prepping...")

  f <- suppressWarnings(formula(paste0(enquo(outcome), " ~ .")))

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
