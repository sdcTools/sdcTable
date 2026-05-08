# Retrieve information in `sdcProblem` or `problemInstance` objects

Function `getInfo()` is used to extract values from `sdcProblem` or
`problemInstance` objects

## Usage

``` r
getInfo(object, type)
```

## Arguments

- object:

  an object of class `sdcProblem` or `problemInstance`

- type:

  a scalar character specifying the information which should be
  returned. If `object` inherits class `problemInstance`, the slots are
  directly accessed, otherwise the values within slot `problemInstance`
  of the `sdcProblem` object are queried. Valid choices are:

  - the object has not yet been protected

    - `lb` and `ub`: current possible lower and upper bounds

    - `LPL`, `SPL`, `UPL`: current lower, sliding and upper protection
      levels

    - `sdcStatus`: current sdc-status of cells

    - `freq`: cell frequencies

    - `strID`: standardized cell ids (chr)

    - `numVars`: `NULL` or a list with a slot for each tabulated
      numerical variable;

    - `w`: sampling weights or `NULL`

  - the table has already been protected

    - `finalData`: protected table as a `data.table`

    - `nrNonDuplicatedCells`: number of unique (non-bogus) cells in the
      table

    - `nrPrimSupps`: number of primary sensitive cells that were
      protected

    - `nrSecondSupps`: number of additional secondary suppressions

    - `nrPublishableCells`: number of cells (status `"s` or \`"z") that
      may be published

    - `suppMethod`: name of the algorithm used to protect the table

## Value

manipulated data depending on arguments `object` and `type`

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>

## Examples

``` r
# define an example problem with two hierarchies
p <- sdc_testproblem(with_supps = FALSE)

# apply primary suppression
p <- primarySuppression(p, type = "freq", maxN = 3)

# `p` is an `sdcProblem` object
print(class(p))
#> [1] "sdcProblem"
#> attr(,"package")
#> [1] "sdcTable"

for (slot in c("lb", "ub", "LPL", "SPL", "UPL", "sdcStatus",
  "freq", "strID", "numVars", "w")) {
  message("slot: ", shQuote(slot))
  print(getInfo(p, type = slot))
}
#> slot: 'lb'
#>  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#> slot: 'ub'
#>  [1] 150 150 150 150 150 150 150 150 150 150 150 150 150 150 150
#> slot: 'LPL'
#>  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#> slot: 'SPL'
#>  [1] 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
#> slot: 'UPL'
#>  [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
#> slot: 'sdcStatus'
#>  [1] "s" "s" "s" "s" "s" "u" "s" "s" "s" "s" "s" "s" "s" "s" "s"
#> slot: 'freq'
#>  [1] 100  55  45  20  18   2  33  14  19  22  12  10  25  11  14
#> slot: 'strID'
#>  [1] "0000" "0001" "0002" "0100" "0101" "0102" "0200" "0201" "0202" "0300"
#> [11] "0301" "0302" "0400" "0401" "0402"
#> slot: 'numVars'
#> $val
#>  [1] 1284  802  482  198  178   20  344  140  204  224  118  106  518  366  152
#> 
#> slot: 'w'
#> NULL

# protect the cell and extract results
p_protected <- protectTable(p, method = "SIMPLEHEURISTIC")
for (slot in c("finalData", "nrNonDuplicatedCells", "nrPrimSupps",
  "nrSecondSupps", "nrPublishableCells", "suppMethod")) {
  message("slot: ", shQuote(slot))
  print(getInfo(p_protected, type = slot))
}
#> slot: 'finalData'
#>     region gender  Freq   val sdcStatus
#>     <char> <char> <num> <num>    <char>
#>  1:  total  total   100  1284         s
#>  2:  total   male    55   802         s
#>  3:  total female    45   482         s
#>  4:      A  total    20   198         s
#>  5:      A   male    18   178         x
#>  6:      A female     2    20         u
#>  7:      B  total    33   344         s
#>  8:      B   male    14   140         s
#>  9:      B female    19   204         s
#> 10:      C  total    22   224         s
#> 11:      C   male    12   118         x
#> 12:      C female    10   106         x
#> 13:      D  total    25   518         s
#> 14:      D   male    11   366         s
#> 15:      D female    14   152         s
#> slot: 'nrNonDuplicatedCells'
#> [1] 15
#> slot: 'nrPrimSupps'
#> [1] 1
#> slot: 'nrSecondSupps'
#> [1] 3
#> slot: 'nrPublishableCells'
#> [1] 11
#> slot: 'suppMethod'
#> [1] "SIMPLEHEURISTIC"
```
