# perform calculations on `cutList`-objects depending on argument `type`

perform calculations on `cutList`-objects depending on argument `type`

## Usage

``` r
calc.cutList(object, type, input)

# S4 method for class 'cutList,character,list'
calc.cutList(object, type, input)
```

## Arguments

- object:

  an object of class `cutList`

- type:

  a character vector of length 1 defining what to
  calculate\|return\|modify. Allowed types are:

&nbsp;

- strengthen: strenghten constraints in argument `object`

- checkViolation: check if a given solution violates any in argument
  `object`

- bindTogether: combine two `cutList`-objects

&nbsp;

- input:

  a list depending on argument `type`.

&nbsp;

- type==strengthen: input is not used (empty list)

- type==checkViolation: input is a list of length 2

  - first element: numeric vector specifying a solution to a linear
    problem

  - second element: numeric vector specifying weights

- type==bindTogether: input is a list of length 1

  - first element: object of class `cutList`

## Value

manipulated data based on argument `type`

- an object of class `cutList` if argument `type` matches 'strengthen'
  or 'bindTogether'

- a logical vector of length 1 if argument `type` matches
  'checkViolation' with TRUE if at least one constraint is violated by
  the given solution

## Note

internal function

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>
