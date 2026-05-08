# modify `cutList`-objects depending on argument `type`

modify `cutList`-objects depending on argument `type`

## Usage

``` r
set.cutList(object, type, input)

# S4 method for class 'cutList,character,list'
set.cutList(object, type, input)
```

## Arguments

- object:

  an object of class `cutList`

- type:

  a character vector of length 1 defining what to
  calculate\|return\|modify. Allowed types are:

&nbsp;

- addCompleteConstraint: add a constraint to argument `object`

- removeCompleteConstraint: remove a constraint from argument `object`

&nbsp;

- input:

  a list depending on argument `type`.

&nbsp;

- type==addCompleteConstraint: a list of length 1

  - first element: an object of class `cutList` with exactly one
    constraint

- type==removeCompleteConstraint: a list of length 1

  - first element: numeric vector of length 1 specifying the index of
    the constraint that should be removed

## Value

an object of class `cutList`

## Note

internal function

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>
