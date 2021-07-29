#' model_selector
#'
#' Determines which models to call.
#'

model_selector <- function(selected_model){
  if(selected_model == "lasso"){
    selcted_model <- model_lasso
  }
  else if (selected_model == "mbic"){
    selected_model <- model_mbic
  }
  else if (selected_model == "mcp"){
    selected_model <- model_mcp
  }
}
