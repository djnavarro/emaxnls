# Package index

## Build

Build Emax regression models

- [`emax_nls()`](https://emaxnls.djnavarro.net/reference/emax_nls.md) :
  Emax model with arbitrary covariates (does not support interactions)
- [`emax_forward()`](https://emaxnls.djnavarro.net/reference/emax_scm.md)
  [`emax_backward()`](https://emaxnls.djnavarro.net/reference/emax_scm.md)
  [`emax_history()`](https://emaxnls.djnavarro.net/reference/emax_scm.md)
  : Stepwise covariate modelling for Emax regression
- [`emax_add_term()`](https://emaxnls.djnavarro.net/reference/emax_update.md)
  [`emax_remove_term()`](https://emaxnls.djnavarro.net/reference/emax_update.md)
  : Add or remove a covariate term from an Emax regression

## Methods

Methods for Emax regression objects

- [`print(`*`<emaxnls>`*`)`](https://emaxnls.djnavarro.net/reference/print.emaxnls.md)
  : Print an Emax regression model object
- [`coef(`*`<emaxnls>`*`)`](https://emaxnls.djnavarro.net/reference/coef.emaxnls.md)
  : Coefficents for an Emax regression
- [`vcov(`*`<emaxnls>`*`)`](https://emaxnls.djnavarro.net/reference/vcov.emaxnls.md)
  : Variance-covariance matrix for an Emax regression
- [`confint(`*`<emaxnls>`*`)`](https://emaxnls.djnavarro.net/reference/confint.emaxnls.md)
  : Confidence intervals for Emax regression model parameters
- [`residuals(`*`<emaxnls>`*`)`](https://emaxnls.djnavarro.net/reference/residuals.emaxnls.md)
  : Residuals for an Emax regression
- [`logLik(`*`<emaxnls>`*`)`](https://emaxnls.djnavarro.net/reference/logLik.emaxnls.md)
  : Log-likelihood for an Emax regression model
- [`AIC(`*`<emaxnls>`*`)`](https://emaxnls.djnavarro.net/reference/AIC.md)
  [`BIC(`*`<emaxnls>`*`)`](https://emaxnls.djnavarro.net/reference/AIC.md)
  : Akaike information criterion / Bayesian information criterion
- [`anova(`*`<emaxnls>`*`)`](https://emaxnls.djnavarro.net/reference/anova.emaxnls.md)
  : Analysis of variance for Emax regression models
- [`simulate(`*`<emaxnls>`*`)`](https://emaxnls.djnavarro.net/reference/simulate.emaxnls.md)
  : Simulate responses from Emax regression model
- [`predict(`*`<emaxnls>`*`)`](https://emaxnls.djnavarro.net/reference/predict.emaxnls.md)
  : Predicting from Emax regression models
- [`deviance(`*`<emaxnls>`*`)`](https://emaxnls.djnavarro.net/reference/deviance.emaxnls.md)
  : Model deviance for an Emax regression
- [`df.residual(`*`<emaxnls>`*`)`](https://emaxnls.djnavarro.net/reference/df.residual.emaxnls.md)
  : Residual degrees of freedom for an Emax regression model
- [`fitted(`*`<emaxnls>`*`)`](https://emaxnls.djnavarro.net/reference/fitted.emaxnls.md)
  : Fitted values for an Emax regression
- [`nobs(`*`<emaxnls>`*`)`](https://emaxnls.djnavarro.net/reference/nobs.emaxnls.md)
  : Number of observations for an Emax regression model
- [`sigma(`*`<emaxnls>`*`)`](https://emaxnls.djnavarro.net/reference/sigma.emaxnls.md)
  : Residual standard deviation for Emax regression models

## Other

Other exported objects

- [`emax_df`](https://emaxnls.djnavarro.net/reference/emax_df.md) :
  Sample simulated data for Emax exposure-response models with
  covariates.
