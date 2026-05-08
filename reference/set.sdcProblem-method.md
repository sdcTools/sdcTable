# modify `sdcProblem`-objects depending on argument `type`

modify `sdcProblem`-objects depending on argument `type`

## Usage

``` r
set.sdcProblem(object, type, input)

# S4 method for class 'sdcProblem,character,list'
set.sdcProblem(object, type, input)
```

## Arguments

- object:

  an object of class `sdcProblem`

- type:

  a character vector of length 1 defining what to
  calculate\|return\|modify. Allowed types are:

&nbsp;

- problemInstance: set\|modify slot 'problemInstance' of argument
  `object`

- partition: set\|modify slot 'partition' of argument `object`

- startI: set\|modify slot 'startI' of argument `object`

- startJ: set\|modify slot 'startJ' of argument `object`

- indicesDealtWith: set\|modify slot 'indicesDealtWith' of argument
  `object`

&nbsp;

- input:

  a list with elements depending on argument `type`.

&nbsp;

- an object of class `problemInstance` if argument `type` matches
  'problemInstance'

- a list (derived from calc.multiple(type='makePartition', ...) if
  argument `type` matches 'partition'

- a numeric vector of length 1 if argument `type` matches 'startI' or
  'startJ'

- a numeric vector if argument `type` matches 'indicesDealtWith'

## Value

an object of class `sdcProblem`

## Note

internal function

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>
