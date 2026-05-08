# query `problemInstance`-objects depending on argument `type`

query `problemInstance`-objects depending on argument `type`

## Usage

``` r
get.problemInstance(object, type)

# S4 method for class 'problemInstance,character'
get.problemInstance(object, type)
```

## Arguments

- object:

  an object of class `problemInstance`

- type:

  a character vector of length 1 defining what to
  calculate\|return\|modify. Allowed types are:

&nbsp;

- strID: vector of unique IDs for each table cell

- nrVars: total number of table cells

- freq: vector of frequencies

- w: a vector of weights used in the linear problem (or NULL)

- numVars: a list containing numeric vectors containing values for
  numerical variables for each table cell (or NULL)

- sdcStatus: a vector containing the suppression state for each cell
  (possible values are 'u': primary suppression, 'x': secondary
  suppression, 'z': forced for publication, 's': publishable cell, 'w':
  dummy cells that are considered only when applying the simple greedy
  heuristic to protect the table)

- lb: lower bound assumed to be known by attackers for each table cell

- ub: upper bound assumed to be known by attackers for each table cell

- LPL: lower protection level required to protect table cells

- UPL: upper protection level required to protect table cells

- SPL: sliding protection level required to protect table cells

- primSupps: vector of indices of primary sensitive cells

- secondSupps: vector of indices of secondary suppressed cells

- forcedCells: vector of indices of cells that must not be suppressed

- hasPrimSupps: shows if `object` has primary suppressions or not

- hasSecondSupps: shows if `object` has secondary suppressions or not

- hasForcedCells: shows if `object` has cells that must not be
  suppressed

- weight: gives weight that is used the suppression procedures

- suppPattern: gives the current suppression pattern

## Value

information from objects of class `dataObj` depending on argument `type`

- a list (or NULL) if argument `type` matches 'numVars'

- numeric vector if argument `type` matches 'freq', 'lb', 'ub', 'LPL',
  'UPL', 'SPL', 'weight', 'suppPattern'

- numeric vector (or NULL) if argument `type` matches 'w', 'primSupps',
  'secondSupps', 'forcedCells'

- character vector if argument `type` matches 'strID', 'sdcStatus', ”

- logical vector of length 1 if argument `type` matches 'hasPrimSupps',
  'hasSecondSupps', 'hasForcedCells'

- numerical vector of length 1 if argument `type` matches 'nrVars'

## Note

internal function

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>
