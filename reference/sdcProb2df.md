# Transform a problem instance

`sdcProb2df()` returns a `data.table` given an
[sdcProblem](https://sdctools.github.io/sdcTable/reference/sdcProblem-class.md)
input object.

## Usage

``` r
sdcProb2df(obj, addDups = TRUE, addNumVars = FALSE, dimCodes = "both")
```

## Arguments

- obj:

  an
  [sdcProblem](https://sdctools.github.io/sdcTable/reference/sdcProblem-class.md)
  object

- addDups:

  (logical), if `TRUE`, duplicated cells are included in the output

- addNumVars:

  (logical), if `TRUE`, numerical variables (if defined in
  [`makeProblem()`](https://sdctools.github.io/sdcTable/reference/makeProblem.md)
  will be included in the output.

- dimCodes:

  (character) allows to specify in which coding the dimensional
  variables should be returned. Possible choices are:

  - `"both"`: both original and internally used, standardized codes are
    included in the output

  - `"original"`: only original codes of dimensional variables are
    included in the output

  - `"default"`: only internally used, standardized codes are included
    in the output

## Value

a `data.table` containing information about all cells of the given
problem

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
