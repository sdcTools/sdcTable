# modify `dataObj`-objects depending on argument `type`

modify `dataObj`-objects depending on argument `type`

## Usage

``` r
set.dataObj(object, type, input)

# S4 method for class 'dataObj,character,listOrNULL'
set.dataObj(object, type, input)
```

## Arguments

- object:

  an object of class `dataObj`

- type:

  a character vector of length 1 defining what to
  calculate\|return\|modify. Allowed types are:

&nbsp;

- rawData: set slot 'rawData' of argument `object`

&nbsp;

- input:

  a list depending on argument `type`.

&nbsp;

- type==rawData: a list containing raw data

## Value

an object of class `dataObj`

## Note

internal function

## Author

Bernhard Meindl <bernhard.meindl@statistik.gv.at>
