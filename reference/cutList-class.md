# S4 class describing a cutList-object

An object of class `cutList` holds constraints that can be extracted and
used as for objects of class
[`linProb-class`](https://sdctools.github.io/sdcTable/reference/linProb-class.md).
An object of class `cutList` consists of a constraint matrix (slot
`con`), a vector of directions (slot `direction`) and a vector
specifying the right hand sides of the constraints (slot `rhs`).

## Details

- slot `con`::

  an object of class
  [`simpleTriplet-class`](https://sdctools.github.io/sdcTable/reference/simpleTriplet-class.md)
  specifying the constraint matrix of the problem

- slot `direction`::

  a character vector holding the directions of the constraints, allowed
  values are:

  - `==`: equal

  - `<`: less

  - `>`: greater

  - `<=`: less or equal

  - `>=`: greater or equal

- slot `rhs`::

  numeric vector holding right hand side values of the constraints

## Note

objects of class `cutList` are dynamically generated (and removed)
during the cut and branch algorithm when solving the secondary cell
suppression problem

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>
