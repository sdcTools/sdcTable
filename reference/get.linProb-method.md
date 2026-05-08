# query `linProb`-objects depending on argument `type`

query `linProb`-objects depending on argument `type`

## Usage

``` r
get.linProb(object, type)

# S4 method for class 'linProb,character'
get.linProb(object, type)
```

## Arguments

- object:

  an object of class `linProb`

- type:

  a character vector of length 1 defining what to
  calculate\|return\|modify. Allowed types are:

&nbsp;

- constraints: constraint matrix of object `linProb`

- direction: directions of the constraints

- rhs: right hand side of the constraints

- objective: objective function

- types: types of the objective variables

- bounds: bounds of the objective variables

## Value

information from objects of class `linProb` depending on `type`

- an object of class `simpleTriplet` if type matches 'constraints'

- a character vector if type matches 'direction' or 'types'

- a numeric vector if type matches 'objective' or 'rhs'

- a list with elements 'lower' and 'upper' if type matches 'bounds'

  - element 'lower': a list with the first element containing indices
    and the second element containing corrsponding lower bounds

  - element 'upper': a list with the first element containing indices
    and the second element containing corrsponding upper bounds

## Note

internal function

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>
