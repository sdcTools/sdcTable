# S4 class describing a problemInstance-object

An object of class `problemInstance` holds the main information that is
required to solve the secondary cell suppression problem.

## Details

- slot `strID`::

  a character vector (or NULL) of ID's identifying table cells

- slot `Freq`::

  a numeric vector (or NULL) of counts for each table cell

- slot `w`::

  a numeric vector (or NULL) of weights that should be used when solving
  the secondary cell suppression problem

- slot `numVars`::

  a list (or NULL) with each element being a numeric vector holding
  values of specified numerical variables for each table cell

- slot `lb`::

  numeric vector (or NULL) holding assumed lower bounds for each table
  cell

- slot `ub`::

  numeric vector (or NULL) holding assumed upper bounds for each table
  cell

- slot `LPL`::

  numeric vector (or NULL) holding required lower protection levels for
  each table cell

- slot `UPL`::

  numeric vector (or NULL) holding required upper protection levels for
  each table cell

- slot `SPL`::

  numeric vector (or NULL) holding required sliding protection levels
  for each table cell

- slot `sdcStatus`::

  character vector (or NULL) holding the current anonymization state for
  each cell.

  - `z`: cell is forced to be published and must not be suppressed

  - `u`: cell has been primary suppressed

  - `x`: cell is a secondary suppression

  - `s`: cell can be published

## Note

objects of class `problemInstance` are used as input for slot
`problemInstance` in class `sdcProblem`

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>
