# S4 class describing a dimInfo-object

An object of class `dimInfo` holds all necessary information about the
dimensional variables defining a hierarchical table that needs to be
protected.

## Details

- slot `dimInfo`::

  a list (or NULL) with all list elements being objects of class
  `dimVar`

- slot `strID`::

  a character vector (or NULL) defining IDs that identify each table
  cell. The ID's are based on (default) codes of the dimensional
  variables defining a cell.

- slot `strInfo`::

  a list object (or NULL) with each list element being a numeric vector
  of length 2 defining the start and end-digit that is allocated by the
  i-th dimensional variable in ID-codes available in slot `strID`

- slot `vNames`::

  a character vector (or NULL) defining the variable names of the
  dimensional variables defining the table structure

- slot `posIndex`::

  a numeric vector (or NULL) holding the position of the dimensional
  variables within slot `rawData` of class `dataObj`

## Note

objects of class `dimInfo` are input for slots in classes `sdcProblem`
and `safeObj`

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>
