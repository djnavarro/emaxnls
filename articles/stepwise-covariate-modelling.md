# Stepwise covariate modelling

``` r

library(emaxnls)
library(tibble)
set.seed(123)
```

Once you can fit a single Emax model, the next practical question is
usually *which covariates belong in the model*. When there are several
candidate covariates and several structural parameters they might act
on, testing every combination by hand is tedious and error-prone. The
`emax_scm_*()` functions automate this with **stepwise covariate
modelling** (SCM): a forward-addition step that greedily adds the most
helpful covariates, an optional backward-elimination step that prunes
terms that no longer earn their place, and a complete history of every
model considered along the way that can serve as an audit log for the
procedure.

This article focuses on continuous outcomes fitted with
[`emax_nls()`](https://emaxnls.djnavarro.net/reference/emax_nls.md), and
then shows that the identical workflow applies to binary outcomes fitted
with
[`emax_logistic()`](https://emaxnls.djnavarro.net/reference/emax_logistic.md).
If you have not fitted an Emax model before, start with the
model-fitting articles; here we assume familiarity with the structural
model and its covariate submodels.

## The building blocks

At the lowest level, covariate modelling is just adding or removing a
single term from a fitted model. Two exported helpers do exactly that,
[`emax_add_term()`](https://emaxnls.djnavarro.net/reference/emax_update.md)
and
[`emax_remove_term()`](https://emaxnls.djnavarro.net/reference/emax_update.md),
each returning a refitted model:

``` r

base_model <- emax_nls(
  structural_model = rsp_1 ~ exp_1,
  covariate_model = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1),
  data = emax_df
)

# add a single covariate term to the baseline parameter
emax_add_term(base_model, E0 ~ cnt_a)
#> Structural model:
#> 
#>   Exposure:       exp_1 
#>   Response:       rsp_1 
#>   Emax type:      hyperbolic 
#>   Response type:  continuous
#> 
#> Covariate model:
#> 
#>   E0:       E0 ~ 1 + cnt_a 
#>   Emax:     Emax ~ 1 
#>   logEC50:  logEC50 ~ 1 
#> 
#> Model fit:
#> 
#>   Observations:         400 
#>   Residual df:          396 
#>   Residual std. error:  0.5108 
#>   AIC:                  603.6 
#> 
#> Coefficients (95% CI):
#> 
#>   label             estimate std_error lower  upper
#> 1 E0_cnt_a             0.486    0.0116 0.463  0.509
#> 2 E0_Intercept         5.05     0.0759 4.91   5.20 
#> 3 Emax_Intercept       9.97     0.112  9.75  10.2  
#> 4 logEC50_Intercept    8.27     0.0394 8.19   8.35 
#> 
#> Use summary() for hypothesis tests.
```

The stepwise functions are built on top of these: they repeatedly
propose add/remove moves, compare each candidate against the current
model, and keep the best one. You rarely need to call
[`emax_add_term()`](https://emaxnls.djnavarro.net/reference/emax_update.md)
or
[`emax_remove_term()`](https://emaxnls.djnavarro.net/reference/emax_update.md)
directly, but it is worth knowing they are what the automated search is
doing under the hood.

## Setting up the search

Two ingredients are needed to drive an SCM run: a **base model** to
start from, and a list of **candidate covariates** to consider.

The base model is an ordinary `emaxnls` fit, typically with no
covariates (just intercepts for each structural parameter):

``` r

base_model
#> Structural model:
#> 
#>   Exposure:       exp_1 
#>   Response:       rsp_1 
#>   Emax type:      hyperbolic 
#>   Response type:  continuous
#> 
#> Covariate model:
#> 
#>   E0:       E0 ~ 1 
#>   Emax:     Emax ~ 1 
#>   logEC50:  logEC50 ~ 1 
#> 
#> Model fit:
#> 
#>   Observations:         400 
#>   Residual df:          397 
#>   Residual std. error:  1.193 
#>   AIC:                  1281 
#> 
#> Coefficients (95% CI):
#> 
#>   label             estimate std_error lower upper
#> 1 E0_Intercept          7.42    0.119   7.19  7.66
#> 2 Emax_Intercept        9.86    0.251   9.37 10.4 
#> 3 logEC50_Intercept     8.16    0.0931  7.97  8.35
#> 
#> Use summary() for hypothesis tests.
```

The candidate list is a named list. Each name is a structural parameter,
and each value is a character vector of covariate names that may be
added to that parameter. Here we allow any of five covariates to act on
either the baseline `E0` or the maximal effect `Emax`:

``` r

candidates <- list(
  E0   = c("cnt_a", "cnt_b", "cnt_c", "bin_d", "bin_e"),
  Emax = c("cnt_a", "cnt_b", "cnt_c", "bin_d", "bin_e")
)
```

This defines a search space of ten candidate terms (five covariates
$`\times`$ two parameters). The `emax_df` dataset was generated so that
only `cnt_a` genuinely affects the continuous response `rsp_1` (through
its baseline), so a well-behaved procedure should select `E0 ~ cnt_a`
and leave the rest out.

## Forward addition piped to backward elimination

The typical workflow is a **forward-backward** run: forward addition to
build the model up, immediately followed by backward elimination to
prune it. Because both functions take a fitted model as their first
argument and return a fitted model, they compose naturally with the
native pipe:

``` r

final_model <- base_model |>
  emax_scm_forward(candidates = candidates, threshold = 0.01, seed = 1) |>
  emax_scm_backward(candidates = candidates, threshold = 0.001, seed = 1)
#> Warning: `nls()` did not converge
```

A few things are worth unpacking here.

**How terms are chosen.** Selection is based on a $`p`$-value criterion.
In the forward step, every candidate term not already in the model is
added in turn and compared against the current model; the term with the
smallest $`p`$-value is retained, provided that $`p`$-value is below
`threshold`. The step repeats until no remaining candidate clears the
bar. Backward elimination works in reverse: each term currently in the
model is dropped in turn, and a term is removed if its $`p`$-value
*exceeds* `threshold`. For continuous models the $`p`$-value comes from
an $`F`$-test comparing the nested models (via
[`anova()`](https://emaxnls.djnavarro.net/reference/anova.md) on the
underlying `nls` fits).

**Why the thresholds differ.** It is standard practice to make forward
addition more permissive than backward elimination — here $`0.01`$ to
add but $`0.001`$ to retain. A term added on the looser forward
criterion must then survive the stricter backward criterion, which
guards against terms that only looked useful in the presence of others.
Using a stricter backward threshold than forward also prevents the
procedure from cycling (adding and removing the same term forever).

**Reproducibility.** Within each step the candidate terms are tested in
a random order, so results can depend on the state of the random number
generator. Passing a `seed` makes a run reproducible, which is what we
do here. (The `seed` argument is currently experimental and may change
in a future release.)

## The final model

The result of the pipeline is an ordinary fitted model object, so all
the usual methods apply. Printing it shows the covariate structure that
survived the search:

``` r

final_model
#> Structural model:
#> 
#>   Exposure:       exp_1 
#>   Response:       rsp_1 
#>   Emax type:      hyperbolic 
#>   Response type:  continuous
#> 
#> Covariate model:
#> 
#>   E0:       E0 ~ 1 + cnt_a 
#>   Emax:     Emax ~ 1 
#>   logEC50:  logEC50 ~ 1 
#> 
#> Model fit:
#> 
#>   Observations:         400 
#>   Residual df:          396 
#>   Residual std. error:  0.5108 
#>   AIC:                  603.6 
#> 
#> Coefficients (95% CI):
#> 
#>   label             estimate std_error lower  upper
#> 1 E0_cnt_a             0.486    0.0116 0.463  0.509
#> 2 E0_Intercept         5.05     0.0759 4.91   5.20 
#> 3 Emax_Intercept       9.97     0.112  9.75  10.2  
#> 4 logEC50_Intercept    8.27     0.0394 8.19   8.35 
#> 
#> Use summary() for hypothesis tests.
```

As expected, the procedure recovered exactly `E0 ~ cnt_a` and nothing
else. You can work with `final_model` exactly as you would with any
[`emax_nls()`](https://emaxnls.djnavarro.net/reference/emax_nls.md) fit
— [`summary()`](https://emaxnls.djnavarro.net/reference/summary.md),
[`confint()`](https://rdrr.io/r/stats/confint.html),
[`predict()`](https://emaxnls.djnavarro.net/reference/predict.md), and
so on all behave normally.

## The audit log

Every model tested during the procedure — not just the ones that were
kept — is recorded inside the returned object.
[`emax_scm_history()`](https://emaxnls.djnavarro.net/reference/emax_scm.md)
extracts this record as a tibble, giving a complete, inspectable trail
of what the search did and why. This is the audit log for the run.

``` r

history <- emax_scm_history(final_model)
names(history)
#>  [1] "iteration"       "attempt"         "step"            "action"         
#>  [5] "term_tested"     "model_tested"    "model_converged" "term_p_value"   
#>  [9] "model_aic"       "model_bic"       "model_updated"
```

The columns are:

- `iteration` — which round of the search the row belongs to (iteration
  `0` is the base model; each forward/backward pass increments it).
- `attempt` — a running counter over every individual model that was
  fitted.
- `step` — `"base model"`, `"forward"`, `"backward"`, or
  `"final model"`.
- `action` — `"add"` or `"remove"` (`NA` for the base and final rows).
- `term_tested` — the covariate term proposed on that row.
- `model_tested` — the full covariate model that was fitted for that
  attempt.
- `model_converged` — whether that candidate model actually converged.
- `term_p_value` — the $`p`$-value used to judge the term.
- `model_aic`, `model_bic` — information criteria for the candidate
  model.
- `model_updated` — `TRUE` on the single attempt within an iteration
  that was accepted as the new current model.

Printing the whole history shows every attempt in order. For an audit
trail it is usually most useful to see all rows at once:

``` r

print(
  history[, c("iteration", "step", "action", "term_tested",
              "term_p_value", "model_aic", "model_updated")],
  n = Inf
)
#> # A tibble: 22 × 7
#>    iteration step        action term_tested term_p_value model_aic model_updated
#>        <int> <chr>       <chr>  <chr>              <dbl>     <dbl> <lgl>        
#>  1         0 base model  NA     NA            NA             1281. NA           
#>  2         1 forward     add    Emax ~ bin…    3.39e-  1     1282. FALSE        
#>  3         1 forward     add    E0 ~ bin_d     3.82e-  1     1282. FALSE        
#>  4         1 forward     add    Emax ~ cnt…    1.31e-  1     1281. FALSE        
#>  5         1 forward     add    E0 ~ cnt_a     3.74e-148      604. TRUE         
#>  6         1 forward     add    E0 ~ cnt_b     8.93e-  1     1283. FALSE        
#>  7         1 forward     add    E0 ~ bin_e     2.78e-  1     1282. FALSE        
#>  8         1 forward     add    E0 ~ cnt_c     6.36e-  1     1283. FALSE        
#>  9         1 forward     add    Emax ~ bin…    2.13e-  1     1282. FALSE        
#> 10         1 forward     add    Emax ~ cnt…    2.24e- 73      951. FALSE        
#> 11         1 forward     add    Emax ~ cnt…    8.46e-  1     1283. FALSE        
#> 12         2 forward     add    E0 ~ cnt_c     6.07e-  1      605. FALSE        
#> 13         2 forward     add    E0 ~ bin_e     9.48e-  1      606. FALSE        
#> 14         2 forward     add    Emax ~ cnt…    7.32e-  1      606. FALSE        
#> 15         2 forward     add    E0 ~ cnt_b     9.93e-  1      606. FALSE        
#> 16         2 forward     add    Emax ~ cnt…    4.92e-  1      605. FALSE        
#> 17         2 forward     add    Emax ~ bin…   NA               NA  FALSE        
#> 18         2 forward     add    Emax ~ bin…    9.40e-  1      606. FALSE        
#> 19         2 forward     add    E0 ~ bin_d     5.39e-  1      605. FALSE        
#> 20         2 forward     add    Emax ~ cnt…    6.09e-  1      605. FALSE        
#> 21         3 backward    remove E0 ~ cnt_a     3.74e-148     1281. FALSE        
#> 22         4 final model NA     NA            NA              604. NA
```

Reading down the `term_p_value` and `model_updated` columns tells the
whole story: in the first forward iteration `E0 ~ cnt_a` had an
overwhelmingly small $`p`$-value and was accepted (`model_updated` is
`TRUE`); a second forward iteration tested the remaining terms but none
cleared the $`0.01`$ threshold, so addition stopped; and the backward
step then tried to drop `E0 ~ cnt_a` but its $`p`$-value was far below
$`0.001`$, so it was retained.

### A compact decision log

The full history is comprehensive but verbose. A common reporting need
is a condensed “decision log” showing only the base model, the moves
that were actually accepted, and the final model. Because the history is
just a data frame, you can build this with ordinary subsetting — no
extra packages required:

``` r

accepted <- history$step %in% c("base model", "final model") |
  (!is.na(history$model_updated) & history$model_updated)

history[accepted, c("iteration", "step", "action", "term_tested",
                    "model_tested", "term_p_value", "model_aic")]
#> # A tibble: 3 × 7
#>   iteration step        action term_tested model_tested   term_p_value model_aic
#>       <int> <chr>       <chr>  <chr>       <chr>                 <dbl>     <dbl>
#> 1         0 base model  NA     NA          E0 ~ 1, Emax …   NA             1281.
#> 2         1 forward     add    E0 ~ cnt_a  E0 ~ 1 + cnt_…    3.74e-148      604.
#> 3         4 final model NA     NA          E0 ~ 1 + cnt_…   NA              604.
```

This is the kind of table you might include in a report to justify the
final covariate model: it records where the search started, each
accepted change with its supporting $`p`$-value and the resulting AIC,
and where it ended. The complete `history` object remains available if a
reviewer wants to see every model that was considered, and it can be
written out for archiving with, for example:

``` r

utils::write.csv(history, "scm_audit_log.csv", row.names = FALSE)
```

## The same workflow for binary outcomes

Nothing about the SCM interface changes for binary outcomes. You start
from an
[`emax_logistic()`](https://emaxnls.djnavarro.net/reference/emax_logistic.md)
base model instead of an
[`emax_nls()`](https://emaxnls.djnavarro.net/reference/emax_nls.md) one,
and the same
[`emax_scm_forward()`](https://emaxnls.djnavarro.net/reference/emax_scm.md)
/
[`emax_scm_backward()`](https://emaxnls.djnavarro.net/reference/emax_scm.md)
pipeline applies. The only difference is internal: for logistic models
the selection $`p`$-value comes from a likelihood-ratio chi-squared test
rather than an $`F`$-test, matching the Bernoulli likelihood used to fit
the model.

``` r

base_logistic <- emax_logistic(
  structural_model = rsp_2 ~ exp_1,
  covariate_model = list(E0 ~ 1, Emax ~ 1, logEC50 ~ 1),
  data = emax_df
)

final_logistic <- base_logistic |>
  emax_scm_forward(candidates = candidates, threshold = 0.01, seed = 1) |>
  emax_scm_backward(candidates = candidates, threshold = 0.001, seed = 1)

final_logistic
#> Structural model:
#> 
#>   Exposure:       exp_1 
#>   Response:       rsp_2 
#>   Emax type:      hyperbolic 
#>   Response type:  binary (logit link)
#> 
#> Covariate model:
#> 
#>   E0:       E0 ~ 1 + cnt_a + bin_d 
#>   Emax:     Emax ~ 1 
#>   logEC50:  logEC50 ~ 1 
#> 
#> Model fit:
#> 
#>   Observations:  400 
#>   Residual df:   395 
#>   Deviance:      316 
#>   AIC:           326 
#> 
#> Coefficients (95% CI):
#> 
#>   label             estimate std_error  lower  upper
#> 1 E0_cnt_a             0.693    0.0817  0.532  0.853
#> 2 E0_bin_d             1.11     0.289   0.543  1.68 
#> 3 E0_Intercept        -5.69     0.624  -6.91  -4.46 
#> 4 Emax_Intercept       7.99     2.15    5.09  16.2  
#> 5 logEC50_Intercept    9.75     0.504   8.89  10.9  
#> 
#> Use summary() for hypothesis tests.
```

The binary response `rsp_2` was generated with genuine effects of *both*
`cnt_a` and `bin_d` on the baseline, and the search recovers both. The
audit log is extracted in exactly the same way:

``` r

logistic_history <- emax_scm_history(final_logistic)

accepted_b <- logistic_history$step %in% c("base model", "final model") |
  (!is.na(logistic_history$model_updated) & logistic_history$model_updated)

logistic_history[accepted_b, c("iteration", "step", "action", "term_tested",
                               "model_tested", "term_p_value", "model_aic")]
#> # A tibble: 4 × 7
#>   iteration step        action term_tested model_tested   term_p_value model_aic
#>       <int> <chr>       <chr>  <chr>       <chr>                 <dbl>     <dbl>
#> 1         0 base model  NA     NA          E0 ~ 1, Emax …    NA             444.
#> 2         1 forward     add    E0 ~ cnt_a  E0 ~ 1 + cnt_…     4.53e-25      339.
#> 3         2 forward     add    E0 ~ bin_d  E0 ~ 1 + cnt_…     8.47e- 5      326.
#> 4         5 final model NA     NA          E0 ~ 1 + cnt_…    NA             326.
```

Here two forward iterations each accepted a term (`E0 ~ cnt_a`, then
`E0 ~ bin_d`), and the backward step retained both.

## Notes and caveats

- **Selection criterion.** The current implementation selects on
  $`p`$-values only. The history records `model_aic` and `model_bic` for
  every candidate, so you can audit the search against information
  criteria even though they are not used to drive it; selection on AIC
  or other criteria may be supported in future.
- **Threshold choice.** The forward and backward thresholds are the main
  levers you control. Stricter thresholds yield sparser models. Keeping
  the backward threshold at or below the forward threshold is
  recommended to avoid cycling.
- **Greediness.** Stepwise search is greedy and is not guaranteed to
  find the globally best subset of covariates. The audit log is valuable
  precisely because it makes the path the search took transparent and
  reproducible.

For fitting and interpreting individual models — continuous or binary —
see the companion model-fitting articles.
