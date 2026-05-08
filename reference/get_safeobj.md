# Query information from protected problem instances

`get_safeobj()` allows to extract information from protected
[sdcProblem](https://sdctools.github.io/sdcTable/reference/sdcProblem-class.md)
instances.

## Usage

``` r
get_safeobj(object, type, ...)
```

## Arguments

- object:

  an object of class
  [sdcProblem](https://sdctools.github.io/sdcTable/reference/sdcProblem-class.md)

- type:

  a character vector defining what should be returned. Possible choices
  are:

  - `"dimInfo`": get infos on dimensional variables that formed the base
    of the protected data

  - `"finalData`": return final data object

  - `"nrNonDuplicatedCells`": total number of cells that are duplicates

  - `"nrPrimSupps`": total number of primary suppressed cells

  - `"nrSecondSupps`": total number of secondary cell suppressions

  - `"nrPublishableCells`": total number of cells that can be published

  - `"suppMethod`": suppression method that has been used

  - `"cellInfo`": extract information about a specific cell

  - `"cellID`": calculate ID of a specific cell defined by level-codes
    and variable names

- ...:

  additional argument required for choices `"cellInfo"` and `"cellID"`

  - `"specs"`: a named character vector with names relating to the names
    of the dimensional variables and values to levels of the
    hierarchies.

  - `"complete"`: if `TRUE`, the entire row is returned in `"cellID"`,
    otherwise only the cell id (number)

  - `"verbose"`: toggles additional output

## Value

the required information.

## Note

internal function

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>
