#' model_selector
#'
#' @name model_selector
#'
#' @description Determines which models to call.
#'
#'
utils::globalVariables(c("selected_model"))
# TODO Add custom model function, user can input their own model structure as long as it adheres to a specific structure of input and output values.
model_selector <- function(selected_model) {
  if (selected_model == "enet") {
    selcted_model <- model_enet
  } else if (selected_model == "lasso") {
    selcted_model <- model_lasso
  } else if (selected_model == "mbic") {
    selected_model <- model_mbic
  } else if (selected_model == "mcp") {
    selected_model <- model_mcp
  }
}
