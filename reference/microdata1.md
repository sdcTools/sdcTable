# Synthetic Microdata (1)

A \`data.frame\` used for examples and problem-generation in various
examples.

## Usage

``` r
data(microdata1)
```

## Format

a \`data.frame\` with \`100\` rows and variables \`region\`, \`gender\`
and \`val\`.

## Examples

``` r
utils::data("microdata1", package = "sdcTable")
head(microdata1)
#>   region gender val
#> 1      C   male   9
#> 2      C   male  11
#> 3      A   male  10
#> 4      A   male  11
#> 5      C   male  18
#> 6      D female  10
```
