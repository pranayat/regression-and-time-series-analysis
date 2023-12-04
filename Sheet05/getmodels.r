#' @param y character string, variable name of dependent variable
#' @param regressor_names vector of character strings of the names of the regressors
#' @return 
#' vector of character strings with the complete model formulas for all possible 
#' combinations of regressors.
getAllModels <- function(y_name, regressor_names){
  # combn(x,m) returns all possible combinations of m elements from vector x
  regressors <-lapply(1:length(regressor_names), combn, x=regressor_names)
  formulas_regressors <- unlist(lapply(regressors, function(combis) {
    rhs_formulas <- apply(combis, MARGIN=2, FUN=paste, collapse="+")
    return(paste0(y_name,"~", rhs_formulas))
  }))
  return(formulas_regressors)
}

#' @param y character string, variable name of dependent variable
#' @param regressor_names vector of character strings of the names of the regressors
#' @return 
#' character string with formula of the model
getModel <- function(y_name, regressor_names){
  return(paste0(y_name, "~", paste(regressor_names, collapse="+")))
}