
# extractors for model and data ------

.extract_data <- function(object) {
  object$data
}

.extract_design <- function(object) {
  object$env$design
}

.extract_nls <- function(object) {
  object$env$model
}

# extractors for variable names ------

.extract_exposure_name <- function(object) {
  object$info$variables$exposure
}

.extract_response_name <- function(object) {
  object$info$variables$response
}

.extract_parameter_names <- function(object) {
  object$info$coefficients
}

.extract_variable_names <- function(object) {
  object$info$variables
}

# extractors for formulae ------

.extract_covariate_formula <- function(object, param = NULL) {
  if (is.null(param)) return(object$formula$covariate)
  object$formula$covariate[[param]]
}

.extract_structural_formula <- function(object) {
  object$formula$structural
}

.extract_nls_formula <- function(object) {
  object$formula$nls
}

# other extractors -------

.extract_model_type <- function(object) {
  object$info$model_type
}
