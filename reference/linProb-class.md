# S4 class describing a linProb-object

An object of class `linProb` defines a linear problem given by the
objective coefficients (slot `objective`), a constraint matrix (slot
`constraints`), the direction (slot `direction`) and the right hand side
(slot `rhs`) of the constraints. Also, allowed lower (slot
`boundsLower`) and upper (slot `boundsUpper`) bounds of the variables as
well as its types (slot `types`) are specified.

## Details

- slot `objective`::

  a numeric vector holding coefficients of the objective function

- slot `constraints`::

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

- slot `boundsLower`::

  a numeric vector holding lower bounds of the objective variables

- slot `boundsUpper`::

  a numeric vector holding upper bounds of the objective variables

- slot `types`::

  a character vector specifying types of the objective variables,
  allowed types are:

  - `C`: binary

  - `B`: continuous

  - `I`: integer

## Note

when solving the problems in the procedure, minimization of the
objective is performed.

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>
