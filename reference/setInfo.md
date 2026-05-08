# Set/Update information in `sdcProblem` or `problemInstance` objects

Function `setInfo()` is used to update values in `sdcProblem` or
`problemInstance` objects

## Usage

``` r
setInfo(object, type, index, input)
```

## Arguments

- object:

  an object of class `sdcProblem` or `problemInstance`

- type:

  a scalar character specifying the kind of information that should be
  changed or modified; if `object` inherits class `problemInstance`, the
  slots are directly changed, otherwise the values within slot
  `problemInstance` are updated. Valid choices are:

  - `lb`: lower possible bounds for the cell

  - `ub`: max. upper bound for the given cell

  - `LPL`: lower protection level

  - `SPL`: sliding protection level

  - `UPL`: upper protection level

  - `sdcStatus`: cell-status

- index:

  numeric vector defining cell-indices for which which values in a
  specified slot should be changed\|modified

- input:

  numeric or character vector depending on argument `type` with its
  length matching the length of argument `index`

  - character vector if type matches 'sdcStatus'

  - a numeric vector if type matches 'lb', 'ub', 'LPL', 'SPL' or 'UPL'

## Value

a `sdcProblem`- or `problemInstance` object

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

Initially in
[`makeProblem()`](https://sdctools.github.io/sdcTable/reference/makeProblem.md),
cells are coded as `"s"` or `"z"`. Their status transitions to `"u"` or
`"x"` as suppression algorithms are applied.

While `setInfo()` or
[`change_cellstatus()`](https://sdctools.github.io/sdcTable/reference/change_cellstatus.md)
allow for manual state modifications,
[`primarySuppression()`](https://sdctools.github.io/sdcTable/reference/primarySuppression.md)
automatically identifies primary sensitive cells and assigns state
`"u"`. In
[`protectTable()`](https://sdctools.github.io/sdcTable/reference/protectTable.md),
additional cells are identified and set to `"x"` to ensure all primary
sensitive cells are adequately protected.

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>

## Examples

``` r
# load example-problem with suppressions
# (same as example from ?primarySuppression)
p <- sdc_testproblem(with_supps = TRUE)

# which is the overall total?
idx <- which.max(getInfo(p, "freq")); idx
#> [1] 1

# we see that the cell with idx = 1 is the overall total and its
# anonymization state of the total can be extracted as follows:
print(getInfo(p, type = "sdcStatus")[idx])
#> [1] "s"

# we want this cell to never be suppressed
p <- setInfo(p, type = "sdcStatus", index = idx, input = "z")

# we can verify this:
print(getInfo(p, type = "sdcStatus")[idx])
#> [1] "z"

# changing slot 'UPL' for all cells
inp <- data.frame(
  strID = getInfo(p, "strID"),
  UPL_old = getInfo(p, "UPL")
)
inp$UPL_new <- inp$UPL_old + 1
p <- setInfo(p, type = "UPL", index = 1:nrow(inp), input = inp$UPL_new)
```
