# A Problem-Instance used for examples/testing

`sdc_testproblem()` returns a sdc-problem instance with `2` hierarchies
and optionally with a single suppressed cell that is used in various
examples and tests.

## Usage

``` r
sdc_testproblem(with_supps = FALSE)
```

## Arguments

- with_supps:

  if `TRUE`, a single cell (violating minimal-frquency rule with `n`
  = 2) is marked as primary sensitive.

## Value

a problem instance

## Examples

``` r
p1 <- sdc_testproblem(); p1
#> The object is a sdcProblem instance with 15 cells in 2 dimension(s)!
#> Protection: no
#> 
#> The dimensions are:
#>  - region (2 levels; 5 codes; of these being 1 aggregates)
#>  - gender (2 levels; 3 codes; of these being 1 aggregates)
#> 
#> Current suppression pattern:
#>  - Primary suppressions: 0
#>  - Secondary suppressions: 0
#>  - Publishable cells: 15
sdcProb2df(p1)
#> Key: <strID>
#>      strID  freq sdcStatus region gender region_o gender_o
#>     <char> <num>    <char> <char> <char>   <char>   <char>
#>  1:   0000   100         s     00     00    total    total
#>  2:   0001    55         s     00     01    total     male
#>  3:   0002    45         s     00     02    total   female
#>  4:   0100    20         s     01     00        A    total
#>  5:   0101    18         s     01     01        A     male
#>  6:   0102     2         s     01     02        A   female
#>  7:   0200    33         s     02     00        B    total
#>  8:   0201    14         s     02     01        B     male
#>  9:   0202    19         s     02     02        B   female
#> 10:   0300    22         s     03     00        C    total
#> 11:   0301    12         s     03     01        C     male
#> 12:   0302    10         s     03     02        C   female
#> 13:   0400    25         s     04     00        D    total
#> 14:   0401    11         s     04     01        D     male
#> 15:   0402    14         s     04     02        D   female

# a single protected cell
p2 <- sdc_testproblem(with_supps = TRUE); p2
#> The object is a sdcProblem instance with 15 cells in 2 dimension(s)!
#> Protection: no
#> 
#> The dimensions are:
#>  - region (2 levels; 5 codes; of these being 1 aggregates)
#>  - gender (2 levels; 3 codes; of these being 1 aggregates)
#> 
#> Current suppression pattern:
#>  - Primary suppressions: 1
#>  - Secondary suppressions: 0
#>  - Publishable cells: 14
sdcProb2df(p2)
#> Key: <strID>
#>      strID  freq sdcStatus region gender region_o gender_o
#>     <char> <num>    <char> <char> <char>   <char>   <char>
#>  1:   0000   100         s     00     00    total    total
#>  2:   0001    55         s     00     01    total     male
#>  3:   0002    45         s     00     02    total   female
#>  4:   0100    20         s     01     00        A    total
#>  5:   0101    18         s     01     01        A     male
#>  6:   0102     2         u     01     02        A   female
#>  7:   0200    33         s     02     00        B    total
#>  8:   0201    14         s     02     01        B     male
#>  9:   0202    19         s     02     02        B   female
#> 10:   0300    22         s     03     00        C    total
#> 11:   0301    12         s     03     01        C     male
#> 12:   0302    10         s     03     02        C   female
#> 13:   0400    25         s     04     00        D    total
#> 14:   0401    11         s     04     01        D     male
#> 15:   0402    14         s     04     02        D   female

# cell status differs in one cell
specs <- c(gender = "female", region = c("A"))
cell_info(p1, specs = specs)
#> Key: <strID>
#>       id  strID region gender  freq   val sdcStatus is_primsupp is_secondsupp
#>    <int> <char> <char> <char> <num> <num>    <char>      <lgcl>        <lgcl>
#> 1:     6   0102      A female     2    20         s       FALSE         FALSE
cell_info(p2, specs = specs)
#> Key: <strID>
#>       id  strID region gender  freq   val sdcStatus is_primsupp is_secondsupp
#>    <int> <char> <char> <char> <num> <num>    <char>      <lgcl>        <lgcl>
#> 1:     6   0102      A female     2    20         u        TRUE         FALSE
```
