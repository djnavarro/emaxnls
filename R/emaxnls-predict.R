
# predict method for nls objects, based on xgxr::predict.nls()
.predict_nls <- function(object, 
                         newdata = NULL, 
                         se.fit = FALSE, 
                         interval = "none", 
                         level = 0.95, 
                         ...) {

  if (is.null(newdata)) {
    # when the user does not specify new data, we can
    # pull the value and gradient directly from the 
    # nls object
    fg <- list(
      value = as.numeric(object$m$fitted()),
      grad = object$m$gradient()
    )

  } else {
    # if newdata is specified the values need to be
    # recomputed, and if standard errors or confidence
    # intervals are requested, the gradient is needed
    fg <- .fgrad(
      form = object$m$formula(), 
      newdata = newdata, 
      pars = object$m$getPars(), 
      with_gradient = (se.fit == TRUE) | (interval == "confidence")
    )
  }

  # if no interval is requested and no standard 
  # errors are requested, return vector of predictions
  if (interval == "none" & se.fit == FALSE) return(fg$value)

  # compute the standard error
  vcov <- vcov(object)
  df <- summary(object)$df[2]
  gs <- rowSums((fg$grad %*% vcov) * fg$grad)
  se <- sqrt(gs)

  # if the standard error is requested with no 
  # interval, return list with three components
  if (interval == "none") return(list(fit = fg$value, se.fit = se, df = df))

  # compute confidence intervals
  alpha <- 1 - level
  deltaf <- se * stats::qt(1 - alpha/2, df = df)
  ci_fit <- data.frame(
    fit = fg$value,
    lwr = fg$value - deltaf,
    upr = fg$value + deltaf
  )

  # if interval is requested but no standard error, 
  # return data frame with three colums
  if (se.fit == FALSE) return(ci_fit)

  # otherwise, return the list
  return(list(fit = ci_fit, se.fit = se, df = df))
}

# function to calculate gradient with respect to model parameters
# - value is the function value
# - grad is the gradient

.fgrad <- function(form, newdata, pars, with_gradient) {

  # import parameters to the local environment
  list2env(x = as.list(pars), envir = environment())

  # setup return object
  ret <- list()
  if (with_gradient) ret$grad <- list()
  
  n_obs <- nrow(newdata)
  n_var <- ncol(newdata)
  vars <- names(newdata)

  for(i in 1:n_obs) { 
    for(v in vars) {
      assign(v, newdata[i,v])
    }
    ret$value[i] <- eval(form[[3L]])
    if (with_gradient) {
      d <- as.list(eval(Deriv::Deriv(form, names(pars), cache.exp = FALSE)))
      ret$grad[[i]] <- d
      if(is.null(names(ret$grad[[i]]))){
        names(ret$grad[[i]]) <- names(pars)
      }
    }
  }

  if (with_gradient) {
    ret$grad <- as.matrix(dplyr::bind_rows(ret$grad))
  }
  return(ret)
}
