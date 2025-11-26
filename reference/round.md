# Round Numbers to a Specified Number of Digits —-

This function rounds numeric values to the specified number of decimal
digits, mimicking the behavior of the base R `round()` function but
implemented manually.

## Usage

``` r
round(x, digits)
```

## Arguments

- x:

  A numeric vector to be rounded.

- digits:

  Integer indicating the number of decimal places to round to.

## Value

A numeric vector rounded to the specified number of digits.

## Examples

``` r
round(3.14159, 2)
#> [1] 3.14
round(c(-2.718, 3.14159), 1)
#> [1] -2.7  3.1
```
