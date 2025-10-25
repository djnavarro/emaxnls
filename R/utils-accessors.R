
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

# accessors for variable names ------

.get_exposure_name <- function(object) {
  object$info$variables$exposure
}

.get_response_name <- function(object) {
  object$info$variables$response
}

.get_coefficient_names <- function(object) {
  object$info$coefficients
}

.get_variable_names <- function(object) {
  object$info$variables
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
  object$formula$nls
}

# other accessors -------

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
