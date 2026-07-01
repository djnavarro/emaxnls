# Package index

## Build Emax NLS models

Build and modify continuous response Emax regression models

- [`emax_nls()`](https://emaxnls.djnavarro.net/reference/emax_nls.md) :
  Estimate parameters for an Emax regression model
- [`emax_nls_options()`](https://emaxnls.djnavarro.net/reference/emax_nls_options.md)
  : Settings used to estimate Emax model
- [`emax_nls_init()`](https://emaxnls.djnavarro.net/reference/emax_nls_init.md)
  : Construct an initial guess for the Emax model parameters

## Build Emax logistic models

Build binary response Emax logistic regression models

- [`emax_logistic()`](https://emaxnls.djnavarro.net/reference/emax_logistic.md)
  : Estimate parameters for a logistic Emax regression model
- [`emax_logistic_options()`](https://emaxnls.djnavarro.net/reference/emax_logistic_options.md)
  : Settings used to estimate a logistic Emax model
- [`emax_logistic_init()`](https://emaxnls.djnavarro.net/reference/emax_logistic_init.md)
  : Construct an initial guess for logistic Emax model parameters

## Covariate selection

Stepwise covariate modeling for Emax regression models

- [`emax_scm_forward()`](https://emaxnls.djnavarro.net/reference/emax_scm.md)
  [`emax_scm_backward()`](https://emaxnls.djnavarro.net/reference/emax_scm.md)
  [`emax_scm_history()`](https://emaxnls.djnavarro.net/reference/emax_scm.md)
  : Stepwise covariate modeling for Emax regression

## Methods for Emax regression objects

S3 methods for `emaxnls` and `emaxlogistic` objects

- [`print(`*`<emaxnls>`*`)`](https://emaxnls.djnavarro.net/reference/print.emaxnls.md)
  : Print an Emax regression model object
- [`print(`*`<emaxlogistic>`*`)`](https://emaxnls.djnavarro.net/reference/print.emaxlogistic.md)
  : Print a logistic Emax regression model object
- [`summary(`*`<emaxnls>`*`)`](https://emaxnls.djnavarro.net/reference/summary.emaxnls.md)
  : Summary of an Emax regression model
- [`summary(`*`<emaxlogistic>`*`)`](https://emaxnls.djnavarro.net/reference/summary.emaxlogistic.md)
  : Summary of a logistic Emax regression model
- [`coef(`*`<emaxnls>`*`)`](https://emaxnls.djnavarro.net/reference/coef.emaxnls.md)
  : Coefficients for an Emax regression
- [`vcov(`*`<emaxnls>`*`)`](https://emaxnls.djnavarro.net/reference/vcov.emaxnls.md)
  : Variance-covariance matrix for an Emax regression
- [`confint(`*`<emaxnls>`*`)`](https://emaxnls.djnavarro.net/reference/confint.emaxnls.md)
  : Confidence intervals for Emax regression model parameters
- [`nobs(`*`<emaxnls>`*`)`](https://emaxnls.djnavarro.net/reference/nobs.emaxnls.md)
  : Number of observations for an Emax regression model
- [`sigma(`*`<emaxnls>`*`)`](https://emaxnls.djnavarro.net/reference/sigma.emaxnls.md)
  : Residual standard deviation for an Emax regression model
- [`residuals(`*`<emaxlogistic>`*`)`](https://emaxnls.djnavarro.net/reference/residuals.md)
  [`residuals(`*`<emaxnls>`*`)`](https://emaxnls.djnavarro.net/reference/residuals.md)
  : Residuals for an Emax regression model
- [`fitted(`*`<emaxlogistic>`*`)`](https://emaxnls.djnavarro.net/reference/fitted.md)
  [`fitted(`*`<emaxnls>`*`)`](https://emaxnls.djnavarro.net/reference/fitted.md)
  : Fitted values for an Emax regression model
- [`predict(`*`<emaxlogistic>`*`)`](https://emaxnls.djnavarro.net/reference/predict.md)
  [`predict(`*`<emaxnls>`*`)`](https://emaxnls.djnavarro.net/reference/predict.md)
  : Predicting from Emax regression models
- [`simulate(`*`<emaxlogistic>`*`)`](https://emaxnls.djnavarro.net/reference/simulate.md)
  [`simulate(`*`<emaxnls>`*`)`](https://emaxnls.djnavarro.net/reference/simulate.md)
  : Simulate responses from an Emax regression model
- [`logLik(`*`<emaxlogistic>`*`)`](https://emaxnls.djnavarro.net/reference/logLik.md)
  [`logLik(`*`<emaxnls>`*`)`](https://emaxnls.djnavarro.net/reference/logLik.md)
  : Log-likelihood for an Emax regression model
- [`AIC(`*`<emaxlogistic>`*`)`](https://emaxnls.djnavarro.net/reference/AIC.md)
  [`BIC(`*`<emaxlogistic>`*`)`](https://emaxnls.djnavarro.net/reference/AIC.md)
  [`AIC(`*`<emaxnls>`*`)`](https://emaxnls.djnavarro.net/reference/AIC.md)
  [`BIC(`*`<emaxnls>`*`)`](https://emaxnls.djnavarro.net/reference/AIC.md)
  : Akaike information criterion / Bayesian information criterion
- [`anova(`*`<emaxlogistic>`*`)`](https://emaxnls.djnavarro.net/reference/anova.md)
  [`anova(`*`<emaxnls>`*`)`](https://emaxnls.djnavarro.net/reference/anova.md)
  : Analysis of variance for Emax regression models
- [`deviance(`*`<emaxlogistic>`*`)`](https://emaxnls.djnavarro.net/reference/deviance.md)
  [`deviance(`*`<emaxnls>`*`)`](https://emaxnls.djnavarro.net/reference/deviance.md)
  : Model deviance for an Emax regression model
- [`df.residual(`*`<emaxlogistic>`*`)`](https://emaxnls.djnavarro.net/reference/df.residual.md)
  [`df.residual(`*`<emaxnls>`*`)`](https://emaxnls.djnavarro.net/reference/df.residual.md)
  : Residual degrees of freedom for an Emax regression model

## Other

Other exported functions and objects

- [`emax_df`](https://emaxnls.djnavarro.net/reference/emax_df.md) :
  Sample simulated data for Emax exposure-response models with
  covariates.
- [`emax_converged()`](https://emaxnls.djnavarro.net/reference/emax_converged.md)
  : Check Emax regression model for convergence status
- [`emax_add_term()`](https://emaxnls.djnavarro.net/reference/emax_update.md)
  [`emax_remove_term()`](https://emaxnls.djnavarro.net/reference/emax_update.md)
  : Add or remove a covariate term from an Emax regression
- [`emax_fun()`](https://emaxnls.djnavarro.net/reference/emax_fun.md) :
  Construct Emax prediction function from model object
