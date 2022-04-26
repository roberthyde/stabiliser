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
#' @import recipes
#' @import dplyr
#'
#'

prep_data <- function(outcome, data, normalise, dummy, impute){
  message("Prepping...")

  f <- formula(paste0(enquo(outcome), " ~ ."))

  recipe <- data %>%
    recipe(f, data=.)

  if (normalise == TRUE) {
    recipe <- recipe %>%
      step_normalize(all_numeric())
  }

  if (dummy == TRUE) {
    recipe <- recipe %>%
      step_dummy(all_nominal())
  }

  data_prepped <- recipe %>%
    prep() %>%
    juice()

  return(data_prepped)
}
