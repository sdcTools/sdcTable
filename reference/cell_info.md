# Get information about specific cells

Function
[`cellInfo()`](https://sdctools.github.io/sdcTable/reference/defunct-sdcTable.md)
can be used to query information of a single cell from a
[sdcProblem](https://sdctools.github.io/sdcTable/reference/sdcProblem-class.md)
object. If the instance has already been protected using
[`protectTable()`](https://sdctools.github.io/sdcTable/reference/protectTable.md),
the information is retrieved from the final protected dataset, otherwise
from the current state of the instance.

## Usage

``` r
cell_info(object, specs, ...)
```

## Arguments

- object:

  an object of class
  [sdcProblem](https://sdctools.github.io/sdcTable/reference/sdcProblem-class.md)

- specs:

  input that defines which cells to query; the function expects either
  (see examples below)

  - a named character vector: with names referring to the names of the
    dimensional variables and the values to its labels. In this case
    each vector-element must contain a single value (label)

  - a `data.frame` where the column-names refer to the names of the
    dimensional variables and the values to the labels

- ...:

  additional parameters for potential future use, currently unused.

## Value

a `data.frame` with a row for each of the queried cells; the object
contains the following columns:

- id: numeric vector of length 1 specifying the numerical index of the
  cell

- a column `strID` if `object` has not yet been protected

- one column for each dimensional variable

- a column `freq` containing the cell-frequencies

- if available, one column for each (possible) numerical value that was
  tabulated

- a column `sdcStatus` with the current sdc code

- is_primsupp: is `TRUE` if the cell is a primary sensitive cell

- is_secondsupp: is `TRUE` if the cell is a secondary suppressed cell

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>

## Examples

``` r
# as in makeProblem() with a single primary suppression
p <- sdc_testproblem(with_supps = TRUE)
sdcProb2df(p)
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

# vector input
specs_vec <- c(region = "D", gender = "male")
cell_info(p, specs = specs_vec)
#> Key: <strID>
#>       id  strID region gender  freq   val sdcStatus is_primsupp is_secondsupp
#>    <int> <char> <char> <char> <num> <num>    <char>      <lgcl>        <lgcl>
#> 1:    14   0401      D   male    11   366         s       FALSE         FALSE

# data.frame input
specs_df <- data.frame(
  region = c("A", "D", "A"),
  gender = c("male", "female", "female")
)
cell_info(p, specs = specs_df)
#>       id  strID region gender  freq   val sdcStatus is_primsupp is_secondsupp
#>    <int> <char> <char> <char> <num> <num>    <char>      <lgcl>        <lgcl>
#> 1:     5   0101      A   male    18   178         s       FALSE         FALSE
#> 2:    15   0402      D female    14   152         s       FALSE         FALSE
#> 3:     6   0102      A female     2    20         u        TRUE         FALSE

# protect the table
p_safe <- protectTable(p, method = "SIMPLEHEURISTIC")

# re-apply
cell_info(p_safe, specs = specs_df)
#>       id  strID region gender  freq   val sdcStatus is_primsupp is_secondsupp
#>    <int> <char> <char> <char> <num> <num>    <char>      <lgcl>        <lgcl>
#> 1:     5   0101      A   male    18   178         x       FALSE          TRUE
#> 2:    15   0402      D female    14   152         s       FALSE         FALSE
#> 3:     6   0102      A female     2    20         u        TRUE         FALSE
```
