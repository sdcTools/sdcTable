# Change anonymization status of a specific cell

Function `change_cellstatus()` allows to change\|modify the
anonymization state of single table cells for objects of class
[sdcProblem](https://sdctools.github.io/sdcTable/reference/sdcProblem-class.md).

## Usage

``` r
change_cellstatus(object, specs, rule, verbose = FALSE, ...)
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

- rule:

  scalar character vector specifying a valid anonymization code ('u',
  'z', 'x', 's') to which all the desired cells under consideration
  should be set.

- verbose:

  scalar logical value defining verbosity, defaults to `FALSE`

- ...:

  additional parameters for potential future use, currently unused.

## Value

a
[sdcProblem](https://sdctools.github.io/sdcTable/reference/sdcProblem-class.md)
object

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>

## Examples

``` r
# load example-problem
# (same as example from ?makeProblem)
p <- sdc_testproblem(with_supps = FALSE)

# goal: set cells with region = "D" and gender != "total" as primary sensitive

# using a data.frame as input
specs <- data.frame(
  region = "D",
  gender = c("male", "female", "total")
)

# marking the cells as sensitive
p <- change_cellstatus(
  object = p,
  specs = specs,
  rule = "u"
)

# check
cell_info(p, specs = specs)
#>       id  strID region gender  freq   val sdcStatus is_primsupp is_secondsupp
#>    <int> <char> <char> <char> <num> <num>    <char>      <lgcl>        <lgcl>
#> 1:    14   0401      D   male    11   366         u        TRUE         FALSE
#> 2:    15   0402      D female    14   152         u        TRUE         FALSE
#> 3:    13   0400      D  total    25   518         u        TRUE         FALSE

# using a named vector for a single cell to revert
# setting D/total as primary-sensitive

specs <- c(gender = "total", region = "D")

p <- change_cellstatus(
  object = p,
  specs = specs,
  rule = "s"
)

# and check again
cell_info(p, specs = specs)
#> Key: <strID>
#>       id  strID region gender  freq   val sdcStatus is_primsupp is_secondsupp
#>    <int> <char> <char> <char> <num> <num>    <char>      <lgcl>        <lgcl>
#> 1:    13   0400      D  total    25   518         s       FALSE         FALSE
```
