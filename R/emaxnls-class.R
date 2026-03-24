
.emax_nls <- function(structural_model,
                      covariate_model,
                      data,
                      init,
                      opts) {

  .validate_structural_formula(structural_model, names(data))
  .validate_covariate_formula(covariate_model, names(data))

  names(covariate_model) <- .map_chr(covariate_model, function(x) as.character(x[[2]]))
  
  if (is.null(opts)) opts <- emax_nls_options()

  tmp <- .construct_design(structural_model, covariate_model, data)

  obj <- list(
    formula = list( 
      structural = structural_model,
      covariate  = covariate_model,
      expanded = NULL
    ),      
    data = data,
    info = list(
      opts = opts,
      init = NULL, 
      design = tmp$design,
      model_type = .construct_model_type(covariate_model),
      variables = .construct_variables(structural_model, covariate_model, tmp$lookup)
    )
  )
  
  if (is.null(init)) init <- .guess_init(obj$info$variables, obj$info$design)
  obj$info$init <- init
  obj$formula$expanded <- .construct_expanded_formula(obj$info$variables)
  obj$env <- .construct_env(obj)


  # estimate the nls model
  tmp <- evalq(
    .nls_call(
      formula   = formula,
      data      = design,
      start     = start,
      control   = control,
      algorithm = algorithm,
      lower     = lower, 
      upper     = upper
    ),
    envir = obj$env
  )

  # store the results within the model environment
  obj$env$model <- tmp$result 
  obj$env$error <- tmp$error

  # message user if needed and return
  if (!is.null(obj$env$error) & !opts$quiet) {
    rlang::warn("`nls()` did not converge", class = "emaxnls_warning")
  }
  return(structure(obj, class = "emaxnls"))
}

.construct_model_type <- function(covariate_model) {
  if (.is_hyperbolic(covariate_model)) return("hyperbolic")
  if (.is_sigmoidal(covariate_model)) return("sigmoidal")
  stop("invalid covariate model", call. = FALSE)
}

.construct_design <- function(structural_model, covariate_model, data) {

  # construct flat formula 
  ss <- deparse(structural_model)
  cc <- unlist(.map(
    .x = covariate_model,
    .f = function(x) all.vars(x[[3]])
  ))
  cc <- paste(cc, collapse = "+")
  if (nchar(cc) == 0) {
    ff <- stats::as.formula(ss)
  } else {
    ff <- stats::as.formula(paste(ss, cc, sep = "+"))
  }

  # model matrix
  mm <- stats::model.matrix(ff, data)

  preds <- all.vars(ff)        # variables required, including response
  terms <- colnames(mm)[-1]    # terms in the model, dropping intercept
  terms <- c(preds[1], terms)  # but keep the response
  terms <- gsub(" ", "", terms)
  index <- attr(mm, "assign")[-1]
  index <- c(1, index + 1)     # index into preds

  lookup <- .tibble(var_name = preds[index], term = terms)
  design <- stats::setNames(.as_tibble(mm), terms)
  design[[preds[1]]] <- data[[preds[1]]]
     
  return(list(design = design, lookup = lookup))
}

# build env from the rest of the object structure: design principle 
# is that everything you need to construct env should be elsewhere
# in the model object
.construct_env <- function(obj) {
  rlang::new_environment(
    data = list(
      formula   = obj$formula$expanded, 
      design    = obj$info$design, 
      start     = obj$info$init$start, 
      control   = obj$info$opts$optim_control, 
      algorithm = .nls_method(obj$info$opts$optim_method), 
      lower     = obj$info$init$lower,
      upper     = obj$info$init$upper
    ), 
    parent = parent.frame()
  )
}

.construct_variables <- function(structural_model, covariate_model, term_lookup) {
  variables <- list(
    response = as.character(structural_model[[2]]),
    exposure = as.character(structural_model[[3]]),
    E0       = as.character(all.vars(covariate_model[["E0"]][[3]])),
    Emax     = as.character(all.vars(covariate_model[["Emax"]][[3]])),
    logEC50  = as.character(all.vars(covariate_model[["logEC50"]][[3]]))
  )
  model_type <- .construct_model_type(covariate_model)
  if (model_type == "sigmoidal") {
    variables$logHill <- as.character(all.vars(covariate_model[["logHill"]][[3]]))
  }
  variables_df_list <- .imap(
    .x = variables, 
    .f = function(x, l) { 
      .tibble(
        param_name = l, 
        var_name = x,
        param_type = .case_when(
          param_name %in% c("exposure", "response") ~ "structural",
          TRUE ~ "covariate"
        )
      )
    }
  )
  variables_df <- do.call(rbind, variables_df_list)
  variables_df <- .right_join(variables_df, term_lookup, by = "var_name")
  variables_df$term <- with(variables_df, .case_when(
    param_type == "structural" ~ NA_character_,
    TRUE ~ term
  ))
  variables_df <- rbind(
    variables_df,
    .tibble(
      var_name = NA_character_,
      param_name = c("E0", "Emax", "logEC50"),
      param_type = "intercept",
      term = as.character(c(1L, 1L, 1L))
    )
  )
  if (model_type == "sigmoidal") {
    variables_df <- rbind(
      variables_df,
      .tibble(
        var_name = NA_character_, 
        param_name = "logHill", 
        param_type = "intercept",
        term = "1"
      )
    )
  }
  variables_df$coef_name <- with(variables_df, .case_when(
    param_type == "structural" ~ NA_character_,
    param_type == "intercept" ~ paste(param_name, "Intercept", sep = "_"),
    param_type == "covariate" ~ paste(param_name, term, sep = "_")
  ))
  return(variables_df)
}

.construct_expanded_formula <- function(variables) {

  model_type <- ifelse("logHill" %in% variables$param_name, "sigmoidal", "hyperbolic")

  rsp_var     <- .filter(variables, param_name == "response")$var_name
  exp_var     <- .filter(variables, param_name == "exposure")$var_name

  cov_e0_term      <- .filter(variables, param_name == "E0")$term
  cov_emax_term    <- .filter(variables, param_name == "Emax")$term
  cov_logec50_term <- .filter(variables, param_name == "logEC50")$term
  cov_loghill_term <- .filter(variables, param_name == "logHill")$term # length 0 for hyperbolic

  cov_e0_coef      <- .filter(variables, param_name == "E0")$coef_name
  cov_emax_coef    <- .filter(variables, param_name == "Emax")$coef_name
  cov_logec50_coef <- .filter(variables, param_name == "logEC50")$coef_name
  cov_loghill_coef <- .filter(variables, param_name == "logHill")$coef_name # length 0 for hyperbolic

  cov_e0_parts      <- .paren(paste(cov_e0_term, cov_e0_coef, sep = " * "))
  cov_emax_parts    <- .paren(paste(cov_emax_term, cov_emax_coef, sep = " * "))
  cov_logec50_parts <- .paren(paste(cov_logec50_term, cov_logec50_coef, sep = " * "))
  cov_loghill_parts <- .paren(paste(cov_loghill_term, cov_loghill_coef, sep = " * "))

  cov_e0_fml      <- .paren(paste(cov_e0_parts, collapse = " + "))
  cov_emax_fml    <- .paren(paste(cov_emax_parts, collapse = " + "))
  cov_logec50_fml <- .paren(paste(cov_logec50_parts, collapse = " + "))
  cov_loghill_fml <- .paren(paste(cov_loghill_parts, collapse = " + "))

  if (model_type == "hyperbolic") {
    nls_fml_txt <- paste0(
      rsp_var, " ~ ", cov_e0_fml, " + ", exp_var,
      " * ", cov_emax_fml, " / (", exp_var, " + exp", cov_logec50_fml, ")"
    )
  }

  if (model_type == "sigmoidal") {
    nls_fml_txt <- paste0(
      rsp_var, " ~ ", cov_e0_fml, " + ",
      exp_var, "^ exp", cov_loghill_fml,
      " * ", cov_emax_fml, " / (",
      exp_var, "^ exp", cov_loghill_fml,
      " + exp", cov_logec50_fml, "^ exp", cov_loghill_fml,
      ")"
    )
  }

  nls_formula <- stats::as.formula(nls_fml_txt)
  return(nls_formula)
}

.paren <- function(x) {
  paste0("(", x, ")")
}

.nls_call <- function(formula, data, start, control, algorithm, lower, upper) {
  if (algorithm %in% c("default", "plinear")) {
    return(.nls_safe(
      formula = formula,
      data = data,
      start = start,
      control = control,
      algorithm = algorithm
    ))
  }
  if (algorithm == "port") {
    return(.nls_safe(
      formula = formula,
      data = data,
      start = start,
      control = control,
      algorithm = algorithm,
      lower = lower,
      upper = upper
    ))
  }
  if (algorithm == "LM") {
    .validate_optim_method("levenberg")
    return(.nls_lm_safe(
      formula = formula,
      data = data,
      start = start,
      control = control,
      algorithm = algorithm
    ))
  } 
}

.nls_method <- function(optim_method) {
  if (optim_method == "gauss") return("default")
  if (optim_method == "port") return("port")
  if (optim_method == "levenberg") return("LM")
  .abort("optim_method must be one of 'gauss', 'port', or 'levenberg'")
}

.nls_lm <- function(...) {
  rlang::check_installed(
    pkg = "minpack.lm",
    reason = "`optim_method = 'levenberg' requires the minpack.lm package"
  )
  minpack.lm::nlsLM(...)
}


