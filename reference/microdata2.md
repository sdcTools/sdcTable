# Synthetic Microdata (2)

Example microdata used for example in \[protect_linked_tables()\].

## Usage

``` r
data(microdata2)
```

## Format

a \`data.frame\` with \`100\` observations containing variables
\`region\`, \`gender\`, \`ecoOld\`, \`ecoNew\` and \`numVal\`.

## Examples

``` r
utils::data("microdata2", package = "sdcTable")
head(microdata2)
#>   region gender ecoOld ecoNew numVal
#> 1     R1      f     Aa     Cc     17
#> 2     R1      f     Bb     Dc     15
#> 3     R1      f     Ab     Cc      5
#> 4     R1      f     Aa     Cb     11
#> 5     R1      f     Bb     Dc     20
#> 6     R1      f     Ab     Cb     10
```
