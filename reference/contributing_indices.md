# Compute contributing units to table cells

This function maps aggregated table cells back to their constituent
microdata. It returns the row indices of the raw input data that
contribute to specific cells identified by `ids`.

## Usage

``` r
contributing_indices(prob, ids = NULL)
```

## Arguments

- prob:

  a
  [sdcProblem](https://sdctools.github.io/sdcTable/reference/sdcProblem-class.md)
  object created with
  [`makeProblem()`](https://sdctools.github.io/sdcTable/reference/makeProblem.md).

- ids:

  a character vector of cell identifiers (`strID`). If `NULL`, indices
  are computed for all cells in the table. Valid identifiers can be
  found in the `strID` column of the data frame returned by
  [`sdcProb2df()`](https://sdctools.github.io/sdcTable/reference/sdcProb2df.md).

## Value

a named list where each element contains integer indices of the
contributing rows in the raw microdata.

## Examples

``` r
# load test problem
p <- sdc_testproblem(with_supps = FALSE)
dt <- sdcProb2df(p, dimCodes = "original")

# find the strID for a specific cell (e.g., region "A" and gender "female")
target_id <- dt[region == "A" & gender == "female", strID]

# compute contributing indices for this cell
contr_list <- contributing_indices(prob = p, ids = target_id)

# view the contributing raw data rows
rawData <- slot(get.sdcProblem(p, "dataObj"), "rawData")
rawData[contr_list[[target_id]]]
#> Key: <region, gender>
#>    region gender  freq tmpsamplingweights   val
#>    <char> <char> <num>              <num> <num>
#> 1:      A female     1                  1     9
#> 2:      A female     1                  1    11

# compute indices for all cells in the table
all_indices <- contributing_indices(p)
```
