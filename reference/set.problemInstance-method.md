# modify `problemInstance`-objects depending on argument `type`

modify `problemInstance`-objects depending on argument `type`

## Usage

``` r
set.problemInstance(object, type, input)

# S4 method for class 'problemInstance,character,list'
set.problemInstance(object, type, input)
```

## Arguments

- object:

  an object of class `problemInstance`

- type:

  a character vector of length 1 defining what to
  calculate\|return\|modify. Allowed types are:

&nbsp;

- lb: set assumed to be known lower bounds

- ub: set assumed to be upper lower bounds

- LPL: set lower protection levels

- UPL: set upper protection levels

- SPL: set sliding protection levels

- sdcStatus: change anonymization status

&nbsp;

- input:

  a list with elements 'indices' and 'values'.

&nbsp;

- element 'indices': numeric vector defining the indices of the cells
  that should be modified

- element 'values': numeric vector whose values are going to replace
  current values for cells defined by 'indices' depending on argument
  `type`

## Value

an object of class `problemInstance`

## Note

internal function

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>
