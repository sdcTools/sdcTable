# query `sdcProblem`-objects depending on argument `type`

query `sdcProblem`-objects depending on argument `type`

## Usage

``` r
get.sdcProblem(object, type)

# S4 method for class 'sdcProblem,character'
get.sdcProblem(object, type)
```

## Arguments

- object:

  an object of class `sdcProblem`

- type:

  a character vector of length 1 defining what to
  calculate\|return\|modify. Allowed types are:

&nbsp;

- dataObj: a list containing the (raw) input data

- problemInstance: return the current problem instance

- partition: a list containing information on the subtables that are
  required to be protected as well as information on the processing
  order of the subtables

- dimInfo: information on the variables defining the hierarchical table

- indicesDealtWith: a set of indices that have already been dealt with
  during the protection algorithmus

- startI: current level at which subtables need to be protected (useful
  when restarting HITAS\|HYPERCUBE)

- startJ: current number of the subtable within a given level that needs
  to be protected (useful when restarting HITAS\|HYPERCUBE)

- innerAndMarginalCellInfo: for a given problem, get indices of inner-
  and marginal table cells

## Value

information from objects of class `sdcProblem` depending on argument
`type`

- an object of class `dataObj` (or NULL) if `type` matches 'dataObj'

- an object of class `problemInstance` (or NULL) if `type` matches
  'problemInstance'

- a list (or NULL) if argument `type` matches 'partition' containing the
  following elements:

  - element 'groups': list with each list-element being a character
    vector specifying a specific level-group

  - element 'indices': list with each list-element being a numeric
    vector defining indices of a subtable

  - element 'strIDs': list with each list-element being a character
    vector defining IDs of a subtable

  - element 'nrGroups': numeric vector of length 1 defining the total
    number of groups that have to be considered

  - element 'nrTables': numeric vector of length 1 defining the total
    number of subtables that have to be considered

- a list (or NULL) if argument `type` matches 'innerAndMarginalCellInfo'
  containing the following elements:

  - element 'innerCells': character vector specifying ID's of inner
    cells

  - element 'totCells': character vector specifying ID's of marginal
    cells

  - element 'indexInnerCells': numeric vector specifying indices of
    inner cells

  - element 'indexTotCells': numeric vector specifying indices of
    marginal cells

- an object of class `dimInfo` (or NULL) if `type` matches 'dimInfo'

- numeric vector of length 1 if argument `type` matches 'startI' or
  'startJ'

## Note

internal function

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>
