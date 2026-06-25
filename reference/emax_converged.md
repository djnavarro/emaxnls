# Check Emax regression model for convergence status

Check Emax regression model for convergence status

## Usage

``` r
emax_converged(mod)
```

## Arguments

- mod:

  An `emaxnls` object

## Value

Logical value

This is a convenience function that takes an Emax regression object as
input. It returns `TRUE` if the optimization routine converged during
model fitting, and `FALSE` if it did not.
