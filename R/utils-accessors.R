
# accessors for model and data ------

.get_data <- function(object) {
  object$data
}

.get_design <- function(object) {
  object$env$design
}

.get_nls <- function(object) {
  object$env$model
}

# accessors for things stored in info ------

.get_exposure_name <- function(object) {
  as.character(object$formula$structural[[3]])
}

.get_response_name <- function(object) {
  as.character(object$formula$structural[[2]])
}

.get_coefficient_names <- function(object) {
  .drop_na(object$info$variables$coef_name)
}

# object can be an emaxnls object or the store
.get_covariate_names <- function(object, param = NULL) {
  var_df <- object$info$variables
  var_df <- .filter(var_df, param_type == "covariate")
  if (is.null(param)) return(var_df$var_name)
  var_df <- .filter(var_df, param_name == param)
  return(var_df$var_name)
}

.get_options <- function(object) {
  object$info$opts
}

.get_model_type <- function(object) {
  object$info$model_type
}

.get_scm_history <- function(object) {
  object$info$history
}

.set_scm_history <- function(object, value) {
  object$info$history <- value
  object
}

# accessors for formulae ------

.get_covariate_formula <- function(object, param = NULL) {
  if (is.null(param)) return(object$formula$covariate)
  object$formula$covariate[[param]]
}

.get_structural_formula <- function(object) {
  object$formula$structural
}

.get_nls_formula <- function(object) {
  object$formula$expanded
}

.get_short_formula <- function(object) {
  fml <- unlist(.map(
    .x = object$formula$covariate,
    .f = deparse
  ))
  fml <- paste(fml, collapse = ", ")
  fml
}

