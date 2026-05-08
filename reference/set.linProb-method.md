# change `linProb`-objects depending on argument `type`

change `linProb`-objects depending on argument `type`

## Usage

``` r
set.linProb(object, type, input)

# S4 method for class 'linProb,character,list'
set.linProb(object, type, input)
```

## Arguments

- object:

  an object of class `linProb`

- type:

  a character vector of length 1 defining what to
  calculate\|return\|modify. Allowed types are:

&nbsp;

- objective: change coefficients of the objective

- direction: change vector of direction of the constraints

- rhs: change vector of right hand side of the constraints

- types: change vector of bounds of the objective variables

- bounds: change bounds of the objective variables

- constraints: change constraint matrix

- removeCompleteConstraint: remove a specific constraint from the object

- addCompleteConstraint: add a constraint to the object

&nbsp;

- input:

  a list depending on argument `type`.

&nbsp;

- type==objective: a list of length 1

  - first element: numeric vector defining coefficients of the objective

- type==direction: a list of length 1

  - first element: character vector defining direction of the
    constraints

- type==rhs: a list of length 1

  - first element: numeric vector defining right hand side of the
    constraints

- type==types: a list of length 1

  - first element: character vector defining types of objective
    variables

- type==bounds: a list of length 2

  - element 'lower': a list with the first element containing indices
    and the second element containing corrsponding lower bounds

  - element 'upper': a list with the first element containing indices
    and the second element containing corrsponding upper bounds

- type==constraints: a list of length 1

  - first element: an object of class `simpleTriplet`

- type==removeCompleteConstraint: a list of length 1

  - first element: numeric vector of length 1 defining the index of the
    constraint that should be removed

- type==addCompleteConstraint: a list of length 1

  - first element: an object of class `cutList` defining the constraint
    that should be added

## Value

an object of class `linProb`

## Note

internal function

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>
