# Create a problem instance

Function `makeProblem()` is used to create
[sdcProblem](https://sdctools.github.io/sdcTable/reference/sdcProblem-class.md)
objects.

## Usage

``` r
makeProblem(
  data,
  dimList,
  dimVarInd = NULL,
  freqVarInd = NULL,
  numVarInd = NULL,
  weightInd = NULL,
  sampWeightInd = NULL
)
```

## Arguments

- data:

  a data frame featuring at least one column for each desired
  dimensional variable. Optionally the input data can feature variables
  that contain information on cell counts, weights that should be used
  during the cut and branch algorithm, additional numeric variables or
  variables that hold information on sampling weights.

- dimList:

  a (named) list where the names refer to variable names in input
  `data`. If the list is not named, it is required to specify argument
  `dimVarInd`. Each list element can be one of:

  - `tree`: generated with `hier_*()` functions from package
    `sdcHierarchies`

  - `data.frame`: a two column `data.frame` containing the full
    hierarchy of a dimensional variable using a top-to-bottom approach.
    The format of this `data.frame` is as follows:

    - **first column:** a character vector specifying levels with each
      vector element being a string only containing of `@`s from length
      1 to n. If a vector element consists of `i`-chars, the
      corresponding code is of level `i`. The code `@` (one character)
      equals the grand total (level=1), the code `@@` (two characters)
      is of level 2 (directly below the overall total).

    - **second column:** a character vector specifying level codes

  - `path`: absolute or relative path to a `.csv` file that contains two
    columns seperated by semicolons (`;`) having the same structure as
    the `"@;levelname"`-structure described above

- dimVarInd:

  if `dimList` is a named list, this argument is ignored (`NULL`). Else
  either a numeric or character vector defining the column indices or
  names of dimensional variables (specifying the table) within argument
  `data` are expected.

- freqVarInd:

  if not `NULL`, a scalar numeric or character vector defining the
  column index or variable name of a variable holding counts in `data`

- numVarInd:

  if not `NULL`, a numeric or character vector defining the column
  indices or variable names of additional numeric variables with respect
  to `data`

- weightInd:

  if not `NULL`, a scalar numeric or character vector defining the
  column index or variable name holding costs within `data` that should
  be used as objective coefficients when solving secondary cell
  suppression problems.

- sampWeightInd:

  if not `NULL`, a scalar numeric or character vector defining the
  column index or variable name of a variable holding sampling weights
  within `data`. In case a complete table is provided, this parameter is
  ignored.

## Value

a
[sdcProblem](https://sdctools.github.io/sdcTable/reference/sdcProblem-class.md)
object

## Cell Status Codes

When an object of class `sdcProblem` is created, every cell is assigned
an internal anonymization state. These codes track the lifecycle of a
cell throughout the suppression process:

- `"s"` (*"Publishable"*): The cell is safe and can be published
  (default).

- `"z"`(*"Forced Publishable"*): The cell must not be suppressed under
  any circumstances.

- `"u"` (\*"Primary Suppressed"): The cell is identified as sensitive
  and requires protection (typically assigned via
  [`primarySuppression()`](https://sdctools.github.io/sdcTable/reference/primarySuppression.md)).

- `"x"` (*"Secondary Suppressed"*): The cell is suppressed to protect
  primary sensitive cells (typically assigned via
  [`protectTable()`](https://sdctools.github.io/sdcTable/reference/protectTable.md)).

- `"w"` (*"Extension Cell"*): The cell exists in the object but will
  never be published. It is treated as a suppressed cell by internal
  algorithms. This is useful if it is known in advance that specific
  codes of a hierarchy will be excluded from publication.

Initially in `makeProblem()`, cells are coded as `"s"` or `"z"`. Their
status transitions to `"u"` or `"x"` as suppression algorithms are
applied.

While
[`setInfo()`](https://sdctools.github.io/sdcTable/reference/setInfo.md)
or
[`change_cellstatus()`](https://sdctools.github.io/sdcTable/reference/change_cellstatus.md)
allow for manual state modifications,
[`primarySuppression()`](https://sdctools.github.io/sdcTable/reference/primarySuppression.md)
automatically identifies primary sensitive cells and assigns state
`"u"`. In
[`protectTable()`](https://sdctools.github.io/sdcTable/reference/protectTable.md),
additional cells are identified and set to `"x"` to ensure all primary
sensitive cells are adequately protected.

## See also

[sdcProb2df](https://sdctools.github.io/sdcTable/reference/sdcProb2df.md)

## Author

Bernhard Meindl

## Examples

``` r
# loading micro data
utils::data("microdata1", package = "sdcTable")

# we can observe that we have a micro data set consisting
# of two spanning variables ('region' and 'gender') and one
# numeric variable ('val')

# specify structure of hierarchical variable 'region'
# levels 'A' to 'D' sum up to a Total
dim.region <- data.frame(
 levels=c('@','@@','@@','@@','@@'),
 codes=c('Total', 'A','B','C','D'),
 stringsAsFactors=FALSE)

# specify structure of hierarchical variable 'gender'
# using create_node() and add_nodes() (see ?manage_hierarchies)
dim.gender <- hier_create(root = "Total", nodes = c("male", "female"))
hier_display(dim.gender)
#> Total
#> ├─male
#> └─female

# create a named list with each element being a data-frame
# containing information on one dimensional variable and
# the names referring to variables in the input data
dimList <- list(region = dim.region, gender = dim.gender)

# third column containts a numeric variable
numVarInd <- 3

# no variables holding counts, numeric values, weights or sampling
# weights are available in the input data
# creating an problem instance using numeric indices
p1 <- makeProblem(
  data = microdata1,
  dimList = dimList,
  numVarInd = 3 # third variable in `data`
)

# using variable names is also possible
p2 <- makeProblem(
  data = microdata1,
  dimList = dimList,
  numVarInd = "val"
)

# what do we have?
print(class(p1))
#> [1] "sdcProblem"
#> attr(,"package")
#> [1] "sdcTable"

# have a look at the data
df1 <- sdcProb2df(p1, addDups = TRUE,
  addNumVars = TRUE, dimCodes = "original")
df2 <- sdcProb2df(p2, addDups=TRUE,
  addNumVars = TRUE, dimCodes = "original")
print(df1)
#> Key: <strID>
#>      strID  freq sdcStatus   val region gender
#>     <char> <num>    <char> <num> <char> <char>
#>  1:   0000   100         s  1284  Total  Total
#>  2:   0001    55         s   802  Total   male
#>  3:   0002    45         s   482  Total female
#>  4:   0100    20         s   198      A  Total
#>  5:   0101    18         s   178      A   male
#>  6:   0102     2         s    20      A female
#>  7:   0200    33         s   344      B  Total
#>  8:   0201    14         s   140      B   male
#>  9:   0202    19         s   204      B female
#> 10:   0300    22         s   224      C  Total
#> 11:   0301    12         s   118      C   male
#> 12:   0302    10         s   106      C female
#> 13:   0400    25         s   518      D  Total
#> 14:   0401    11         s   366      D   male
#> 15:   0402    14         s   152      D female

identical(df1, df2)
#> [1] TRUE
```
