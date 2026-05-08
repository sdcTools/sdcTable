# query `cutList`-objects depending on argument `type`

query `cutList`-objects depending on argument `type`

## Usage

``` r
get.cutList(object, type)

# S4 method for class 'cutList,character'
get.cutList(object, type)
```

## Arguments

- object:

  an object of class `cutList`

- type:

  a character vector of length 1 defining what to
  calculate\|return\|modify. Allowed types are:

&nbsp;

- constraints: constraint matrix of object

- direction: directions of the constraints

- rhs: right hand side of the constraints

- nrConstraints: total number of constraints

## Value

information from objects of class `cutList` depending on argument `type`

- object of class `simpleTriplet` if argument `type` matches
  'constraints'

- character vector if argument `type` matches 'direction'

- numeric vector if argument `type` matches 'objective' or 'rhs'

## Note

internal function

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>
