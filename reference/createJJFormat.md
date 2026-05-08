# Create input for jj_format

This function transforms a
[sdcProblem](https://sdctools.github.io/sdcTable/reference/sdcProblem-class.md)
object into a list that can be used as input for
[`writeJJFormat()`](https://sdctools.github.io/sdcTable/reference/writeJJFormat.md)
to write a problem in `"JJ-format"` to disk.

## Usage

``` r
createJJFormat(x)
```

## Arguments

- x:

  a
  [sdcProblem](https://sdctools.github.io/sdcTable/reference/sdcProblem-class.md)
  object

## Value

an input suitable for
[`writeJJFormat()`](https://sdctools.github.io/sdcTable/reference/writeJJFormat.md)

## Author

Bernhard Meindl (bernhard.meindl@statistik.gv.at) and Sapphire Yu Han
(y.han@cbs.nl)

## Examples

``` r
# setup example problem
# microdata
utils::data("microdata1", package = "sdcTable")

# create hierarchies
dims <- list(
  region = sdcHierarchies::hier_create(root = "Total", nodes = LETTERS[1:4]),
  gender = sdcHierarchies::hier_create(root = "Total", nodes = c("male", "female")))

# create a problem instance
p <- makeProblem(
  data = microdata1,
  dimList = dims,
  numVarInd = "val")

# create suitable input for `writeJJFormat`
inp <- createJJFormat(p); inp
#> [[1]]
#> [1] 0
#> 
#> [[2]]
#> [1] 15
#> 
#> [[3]]
#>       ind   val freqs costs status   lbi   ubi   LPL   UPL   SPL
#>     <num> <num> <num> <num> <char> <num> <num> <num> <num> <num>
#>  1:     0  1284   100   100      s     0   150     1     1     0
#>  2:     1   802    55    55      s     0   150     1     1     0
#>  3:     2   482    45    45      s     0   150     1     1     0
#>  4:     3   198    20    20      s     0   150     1     1     0
#>  5:     4   178    18    18      s     0   150     1     1     0
#>  6:     5    20     2     2      s     0   150     1     1     0
#>  7:     6   344    33    33      s     0   150     1     1     0
#>  8:     7   140    14    14      s     0   150     1     1     0
#>  9:     8   204    19    19      s     0   150     1     1     0
#> 10:     9   224    22    22      s     0   150     1     1     0
#> 11:    10   118    12    12      s     0   150     1     1     0
#> 12:    11   106    10    10      s     0   150     1     1     0
#> 13:    12   518    25    25      s     0   150     1     1     0
#> 14:    13   366    11    11      s     0   150     1     1     0
#> 15:    14   152    14    14      s     0   150     1     1     0
#> 
#> [[4]]
#> [1] 8
#> 
#> [[5]]
#>        v1     v2     v3                               v4
#>    <char> <char> <char>                           <char>
#> 1:    0.0      5      :  0 (-1) 3 (1) 6 (1) 9 (1) 12 (1)
#> 2:    0.0      5      : 1 (-1) 4 (1) 7 (1) 10 (1) 13 (1)
#> 3:    0.0      5      : 2 (-1) 5 (1) 8 (1) 11 (1) 14 (1)
#> 4:    0.0      3      :               0 (-1) 1 (1) 2 (1)
#> 5:    0.0      3      :               3 (-1) 4 (1) 5 (1)
#> 6:    0.0      3      :               6 (-1) 7 (1) 8 (1)
#> 7:    0.0      3      :             9 (-1) 10 (1) 11 (1)
#> 8:    0.0      3      :            12 (-1) 13 (1) 14 (1)
#> 
#> attr(,"numvars")
#> [1] "val"
#> attr(,"class")
#> [1] "jjformat"

# write files to disk
# frequency table by default
writeJJFormat(
  x = inp,
  path = file.path(tempdir(), "prob_freqs.jj"),
  overwrite = TRUE
)
#> File '/tmp/RtmpVEAcDD/prob_freqs.jj' successfully written.

# or using the numeric variable `val` previously specified
writeJJFormat(
  x = inp,
  tabvar = "val",
  path = file.path(tempdir(), "prob_val.jj"),
  overwrite = TRUE
)
#> File '/tmp/RtmpVEAcDD/prob_val.jj' successfully written.
```
