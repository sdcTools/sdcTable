# initialize `cutList`-objects depending on argument `type`

initialize `cutList`-objects depending on argument `type`

## Usage

``` r
init.cutList(type, input)

# S4 method for class 'character,list'
init.cutList(type, input)
```

## Arguments

- type:

  a character vector of length 1 defining what\|how to initialize.
  Allowed types are:

&nbsp;

- empty: create an empty `cutList`-object

- singleCut: create a `cutList`-object with exactly one constraint

- multipleCuts: create a `cutList`-object with more than one constraint

&nbsp;

- input:

  a list depending on argument `type`.

&nbsp;

- type==empty: input is not used (empty list)

- type==singleCut: input is a list of length 3

  - first element: numeric vector specifying a values for the row of the
    constraint matrix that must be created

  - second element: character vector of length 1 specifying the
    direction

  - third element: numeric vector of length 1 specifying the right hand
    side of the constraint

- type==multipleCuts: input is a list of length 3

  - first element: object of class `matrix`

  - second element: character vector specifying the direction of the
    constraints

  - third element: numeric vector specifying the right hand side of the
    constraints

## Value

an object of class `cutList`

## Note

internal function

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>
